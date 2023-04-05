module ImageProcessing.ImageProcessing

open System
open Brahma.FSharp
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

[<RequireQualifiedAccess>]
type Transformation =
    | Blur
    | Edges
    | HighPass
    | Laplacian
    | SobelV
    | Rotate
    | RotateCCW
    | ReflectH
    | ReflectV

    static member all =
        let cases = FSharpType.GetUnionCases(typeof<Transformation>)

        [| for case in cases do
               let transformation = FSharpValue.MakeUnion(case, [||]) :?> Transformation
               yield transformation |]

type RotationDirection =
    | Clockwise
    | Counterclockwise

    override this.ToString() =
        match this with
        | Clockwise -> "Clockwise"
        | Counterclockwise -> "Counterclockwise"

type ReflectionDirection =
    | Horizontal
    | Vertical

    override this.ToString() =
        match this with
        | Horizontal -> "Horizontal"
        | Vertical -> "Vertical"

[<RequireQualifiedAccess>]
type EditType =
    | Transformation of float32[,]
    | Rotation of RotationDirection
    | Reflection of ReflectionDirection

    override this.ToString() =
        match this with
        | Transformation _ -> "Transformation"
        | Rotation direction -> $"%s{direction.ToString()} Rotation"
        | Reflection direction -> $"%s{direction.ToString()} Reflection"

[<RequireQualifiedAccess>]
type Kernel =

    static member makeRotationKernel (clContext: ClContext) localWorkSize =
        let kernel =
            <@
                fun (r: Range1D) (img: ClArray<byte>) height width (result: ClArray<byte>) direction ->
                    let p = r.GlobalID0
                    let pi = p / width
                    let pj = p % width

                    if pi < height && pj < width then
                        if direction > 0 then
                            result[pj * height + height - pi - 1] <- img[pi * width + pj]
                        else
                            result[(width - pj - 1) * height + pi] <- img[pi * width + pj]
            @>

        let kernel = clContext.Compile kernel

        fun direction (commandQueue: MailboxProcessor<_>) (img: ClArray<byte>) height width (result: ClArray<byte>) ->

            let ndRange = Range1D.CreateValid(height * width, localWorkSize)

            let kernel = kernel.GetKernel()

            commandQueue.Post(
                Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img height width result direction)
            )

            commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
            result

    static member makeReflectionKernel (clContext: ClContext) localWorkSize =
        let kernel =
            <@
                fun (r: Range1D) (img: ClArray<byte>) height width (result: ClArray<byte>) direction ->
                    let p = r.GlobalID0
                    let pi = p / width
                    let pj = p % width

                    if pi <= height / 2 && pj <= width / 2 then
                        if direction > 0 then
                            result[pi * width + pj] <- img[(height - 1) * width - (pi * width) + pj]
                            result[(height - 1) * width - (pi * width) + pj] <- img[pi * width + pj]

                            result[pi * width + width - 1 - pj] <-
                                img[(height - 1) * width - (pi * width) + width - 1 - pj]

                            result[(height - 1) * width - (pi * width) + width - 1 - pj] <-
                                img[pi * width + width - 1 - pj]
                        else
                            result[pi * width + pj] <- img[pi * width + width - 1 - pj]
                            result[pi * width + width - 1 - pj] <- img[pi * width + pj]

                            result[(height - 1) * width - (pi * width) + pj] <-
                                img[(height - 1) * width - (pi * width) + width - 1 - pj]

                            result[(height - 1) * width - (pi * width) + width - 1 - pj] <-
                                img[(height - 1) * width - (pi * width) + pj]
            @>

        let kernel = clContext.Compile kernel

        fun direction (commandQueue: MailboxProcessor<_>) (img: ClArray<byte>) height width (result: ClArray<byte>) ->

            let ndRange = Range1D.CreateValid(height * width, localWorkSize)

            let kernel = kernel.GetKernel()

            commandQueue.Post(
                Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img height width result direction)
            )

            commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
            result

    static member makeFilterKernel (clContext: ClContext) localWorkSize =
        let kernel =
            <@
                fun (r: Range1D) (img: ClArray<byte>) width height (filter: ClArray<float32>) filterD (result: ClArray<byte>) ->
                    let p = r.GlobalID0
                    let ph = p / width
                    let pw = p % width
                    let mutable res = 0.0f

                    for i in ph - filterD .. ph + filterD do
                        for j in pw - filterD .. pw + filterD do
                            let mutable d = 0uy

                            if i < 0 || i >= height || j < 0 || j >= width then
                                d <- img[p]
                            else
                                d <- img[i * width + j]

                            let f = filter[(i - ph + filterD) * (2 * filterD + 1) + (j - pw + filterD)]
                            res <- res + (float32 d) * f

                    result[p] <- byte (int res)
            @>

        let kernel = clContext.Compile kernel

        fun (commandQueue: MailboxProcessor<_>) (filter: ClArray<float32>) filterD (img: ClArray<byte>) height width (result: ClArray<byte>) ->

            let ndRange = Range1D.CreateValid(height * width, localWorkSize)

            let kernel = kernel.GetKernel()

            commandQueue.Post(
                Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img width height filter filterD result)
            )

            commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
            result

type VirtualArray<'A>(memory: array<'A>, head: int, length: int) =
    // When an instance is created, check that it is within the specified memory limits
    do
        if head + length - 1 >= memory.Length then
            failwith
                $"Failed to allocate required memory: %A{length} for VirtualArray at the specified starting index: %A{head}"

    member this.Memory = memory
    member this.Head = head
    member this.Length = length
    member this.IsEmpty = length = 0

    member this.Item
        with get i =
            if head + i >= memory.Length then
                failwith "VirtualArray.get: Index out of bounds of the general memory"
            else
                this.Memory[this.Head + i]
        and set i value =
            if head + i >= memory.Length then
                failwith "VirtualArray.set: Index out of bounds of the general memory"
            else
                this.Memory[ this.Head + i ] <- value

    static member splitInto count (vArray: VirtualArray<'A>) =
        if count <= 0 then
            failwith $"VirtualArray.SplitIntoCount count argument: {count} must be positive"

        let len = vArray.Length

        if len = 0 then
            [||]
        else
            let count = min count len
            let res = Array.zeroCreate count
            let minChunkSize = len / count
            let mutable startIndex = vArray.Head

            for i in 0 .. len % count - 1 do
                res[i] <- VirtualArray(vArray.Memory, startIndex, minChunkSize + 1)
                startIndex <- startIndex + minChunkSize + 1

            for i in len % count .. count - 1 do
                res[i] <- VirtualArray(vArray.Memory, startIndex, minChunkSize)
                startIndex <- startIndex + minChunkSize

            res

    static member mirror(arr: array<'A>) = VirtualArray(arr, 0, arr.Length)

    static member fold2 folder (state: 'State) (vArray1: VirtualArray<'A>) (vArray2: VirtualArray<'B>) =
        if vArray1.Length <> vArray2.Length then
            failwith $"Invalid argument vArray1.Length: %A{vArray1.Length} vArray2.Length: %A{vArray2.Length}"

        let mutable state = state

        for i in 0 .. vArray1.Length - 1 do
            state <- folder state vArray1[i] vArray2[i]

        state

    /// Writes the result of an applied function to the output
    static member iteri2 action (vArray: VirtualArray<'A>) (output: array<'A>) =
        for i in 0 .. vArray.Length - 1 do
            output[vArray.Head + i] <- action (vArray.Head + i) vArray.Memory[vArray.Head + i]


[<Struct>]
type Image =
    val Data: array<byte>
    val VirtualData: VirtualArray<byte>
    val Width: int
    val Height: int
    val Name: string

    new(data, virtualData, width, height, name) =
        { Data = data
          VirtualData = virtualData
          Width = width
          Height = height
          Name = name }

    new(data, width, height, name) =
        { Data = data
          VirtualData = VirtualArray.mirror data
          Width = width
          Height = height
          Name = name }

// Convert 2D-array to 1D-array
let flattenArray2D array2D =
    [| for x in 0 .. (Array2D.length1 array2D) - 1 do
           for y in 0 .. (Array2D.length2 array2D) - 1 do

               yield array2D[x, y] |]

type ApplyTransform(?parallelLevel) =

    // Get optimal parameters for parallel computations
    let parallelLevel = defaultArg parallelLevel 1

    /// Apply a given transformation using CPU resources
    member _.OnCPU parameter (img: Image) =

        // Store image dimensions, because we might need to swap them later if Rotation is performed
        let mutable width = img.Width
        let mutable height = img.Height

        // The resulting pixel data depends on the matching case of the transformation parameter
        let result =
            match parameter with
            | EditType.Rotation rotationDirection ->
                /// Function performs a pixel remapping logic to rotate an image
                let remapPixels (input: VirtualArray<byte>) (output: array<byte>) =
                    // Going through each of VirtualArray's indices...
                    for i in input.Head .. input.Head + input.Length - 1 do

                        // ... compute their global memory indices...
                        let pi = i / img.Width
                        let pj = i % img.Width

                        // ... and perform remapping on global memory
                        match rotationDirection with
                        | Clockwise ->
                            output[pj * img.Height + img.Height - pi - 1] <- input.Memory[pi * img.Width + pj]
                        | Counterclockwise ->
                            output[(img.Width - pj - 1) * img.Height + pi] <- input.Memory[pi * img.Width + pj]

                // We create a Virtual and an output buffer array out of a given image's data
                let input = VirtualArray(img.Data, 0, img.Height * img.Width)
                let output = Array.zeroCreate (width * height)

                if parallelLevel = 1 then
                    // For non-async computations on the main thread we just pass the whole VirtualArray
                    remapPixels input output
                else
                    // For async computations we split a given data between processors.
                    // Each performs its own pixel remapping logic.
                    let input = VirtualArray.splitInto parallelLevel input
                    Array.Parallel.iter (fun vArray -> remapPixels vArray output) input

                // Swap dimensions
                width <- height
                height <- img.Width

                // Bind output to the result
                output

            | EditType.Reflection reflectionDirection ->
                /// Function performs a pixel remapping logic to reflect an image
                let remapPixels (input: VirtualArray<byte>) (output: array<byte>) =
                    // Going through each of VirtualArray's indices...
                    for i in input.Head .. input.Head + input.Length - 1 do

                        // ... compute their global memory indices...
                        let pi = i / img.Width
                        let pj = i % img.Width

                        // ... and perform remapping on global memory
                        match reflectionDirection with
                        | Horizontal ->
                            // nw <- sw
                            output[pi * img.Width + pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + pj]
                            // nw -> sw
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + pj] <-
                                input.Memory[pi * img.Width + pj]
                            // ne <- se
                            output[pi * img.Width + img.Width - 1 - pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj]
                            // ne -> se
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj] <-
                                input.Memory[pi * img.Width + img.Width - 1 - pj]

                        | Vertical ->
                            // nw <- ne
                            output[pi * img.Width + pj] <- input.Memory[pi * img.Width + img.Width - 1 - pj]
                            // nw -> ne
                            output[pi * img.Width + img.Width - 1 - pj] <- input.Memory[pi * img.Width + pj]
                            // sw <- se
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj]
                            // sw -> se
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + pj]

                // We create a Virtual array and an output buffer out of a given image's data
                let input = VirtualArray(img.Data, 0, img.Height * img.Width)
                let output = Array.zeroCreate (img.Height * img.Width)

                if parallelLevel = 1 then
                    // For non-async computations on the main thread we just pass the whole VirtualArray
                    remapPixels input output
                else
                    // For async computations we split a given data between processors.
                    // Each performs its own pixel remapping logic.
                    let input = VirtualArray.splitInto parallelLevel input
                    Array.Parallel.iter (fun vArray -> remapPixels vArray output) input

                // Bind output to the result
                output

            | EditType.Transformation filter ->

                // Filter parameters
                let filterD = (Array2D.length1 filter) / 2

                // Flatten 2dArray of pixels
                let filter = flattenArray2D filter

                // Pixel processing logic
                let processPixel p =
                    let pi = p / width
                    let pj = p % width

                    let dataToHandle =
                        [| for i in pi - filterD .. pi + filterD do
                               for j in pj - filterD .. pj + filterD do
                                   if i < 0 || i >= height || j < 0 || j >= width then
                                       float32 img.Data[p]
                                   else
                                       float32 img.Data[i * width + j] |]

                    // Returns weighted sum of pixels
                    Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

                // Create a Virtual array and an output buffer out of a given image's data
                let input = VirtualArray(img.Data, 0, img.Height * img.Width)
                let output = Array.zeroCreate (width * height)

                if parallelLevel = 1 then
                    // For non-async computations on the main thread we just pass the whole VirtualArray
                    VirtualArray.iteri2 (fun i _ -> byte (processPixel i)) input output
                    output

                else
                    // For async computations we split a given data between processors.
                    // Each performs its own pixel remapping logic.
                    let input = VirtualArray.splitInto parallelLevel input
                    let action = VirtualArray.iteri2 (fun i _ -> byte (processPixel i))
                    Array.Parallel.iter (fun (vArray: VirtualArray<byte>) -> action vArray output) input
                    output

        // Return the final result
        Image(result, width, height, img.Name)

    /// Apply a given transformation using GPU resources
    member _.OnGPU (clContext: ClContext) localWorkSize =

        let queue = clContext.QueueProvider.CreateQueue()

        // The lambda-function will be returned
        fun (parameter: EditType) (img: Image) ->

            // Store image dimensions, because we might need to swap them later if Rotation is performed
            let mutable width = img.Width
            let mutable height = img.Height

            let input = clContext.CreateClArray<_>(img.Data, HostAccessMode.NotAccessible)

            let output =
                clContext.CreateClArray(
                    img.Data.Length,
                    HostAccessMode.NotAccessible,
                    allocationMode = AllocationMode.Default
                )

            let (output: ClArray<byte>) =
                match parameter with
                | EditType.Rotation rotationDirection ->
                    let kernel =
                        if rotationDirection = Clockwise then
                            Kernel.makeRotationKernel clContext localWorkSize 1
                        else
                            Kernel.makeRotationKernel clContext localWorkSize 0

                    // Out of scope assignment for correct image dimensions when saving rotated images
                    height <- img.Width
                    width <- img.Height

                    kernel queue input img.Height img.Width output

                | EditType.Reflection reflectionDirection ->
                    let kernel =
                        if reflectionDirection = Horizontal then
                            Kernel.makeReflectionKernel clContext localWorkSize 1
                        else
                            Kernel.makeReflectionKernel clContext localWorkSize 0

                    kernel queue input img.Height img.Width output

                | EditType.Transformation table ->
                    let filterD = (Array2D.length1 table) / 2
                    let filter = flattenArray2D table

                    let clFilter =
                        clContext.CreateClArray<_>(filter, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

                    let kernel = Kernel.makeFilterKernel clContext localWorkSize
                    let res = kernel queue clFilter filterD input img.Height img.Width output
                    // Memory clean-up on GPU
                    queue.Post(Msg.CreateFreeMsg clFilter)
                    res

            let result = Array.zeroCreate (img.Height * img.Width)

            let result = queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(output, result, ch))

            // Memory clean-up on GPU
            queue.Post(Msg.CreateFreeMsg input)
            queue.Post(Msg.CreateFreeMsg output)

            Image(result, width, height, img.Name)

let loadAsImage (file: string) =
    let img = Image.Load<L8> file

    let buf = Array.zeroCreate<byte> (img.Width * img.Height)

    img.CopyPixelDataTo(Span<byte> buf)
    Image(buf, img.Width, img.Height, System.IO.Path.GetFileName file)

let saveImage (image: Image) file =
    let img = Image.LoadPixelData<L8>(image.Data, image.Width, image.Height)
    img.Save file

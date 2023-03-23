module ImageProcessing.ImageProcessing

open System
open Brahma.FSharp
open Microsoft.FSharp.Core
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

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
    | Transformation of float32[][]
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

                    if p < height * width then
                        let pi = p / width
                        let pj = p % width

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

                    if p <= height * width / 2 then
                        let pi = p / width
                        let pj = p % width

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

    /// Mapi from a virtual memory to a general memory of itself
    static member mapi mapping (vArray: VirtualArray<'A>) =
        for i in 0 .. vArray.Length - 1 do
            vArray.Memory[ vArray.Head + i ] <- mapping (vArray.Head + i) vArray.Memory[vArray.Head + i]

        vArray


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

type ApplyTransform =

    static member onCPU parameter (img: Image) =

        // Store image dimensions, because we might need to swap them later if Rotation is performed
        let mutable width = img.Width
        let mutable height = img.Height

        // The resulting pixel data depends on matching case of transformation parameter
        let res =
            match parameter with
            | EditType.Rotation direction ->

                // Resulting buffer
                let res = Array.zeroCreate (width * height)

                // Pixel remapping logic
                if direction = Clockwise then
                    for i in 0 .. height - 1 do
                        for j in 0 .. width - 1 do
                            res[j * height + height - i - 1] <- img.Data[i * width + j]
                else
                    // Counterclockwise
                    for i in 0 .. height - 1 do
                        for j in 0 .. width - 1 do
                            res[(width - j - 1) * height + i] <- img.Data[i * width + j]

                // Swap dimensions
                width <- height
                height <- img.Width

                // Return the result
                res

            | EditType.Reflection reflectionDirection ->

                // Resulting buffer
                let res = Array.zeroCreate (width * height)

                // Pixel remapping logic
                if reflectionDirection = Horizontal then
                    for i in 0 .. (height - 1) / 2 do
                        for j in 0 .. (width - 1) / 2 do
                            // nw <- sw
                            res[i * width + j] <- img.Data[(height - 1) * width - (i * width) + j]
                            // sw <- nw
                            res[(height - 1) * width - (i * width) + j] <- img.Data[i * width + j]
                            // ne <- se
                            res[i * width + width - 1 - j] <-
                                img.Data[(height - 1) * width - (i * width) + width - 1 - j]
                            // se <- ne
                            res[(height - 1) * width - (i * width) + width - 1 - j] <-
                                img.Data[i * width + width - 1 - j]
                else
                    // Vertical
                    for i in 0 .. (height - 1) / 2 do
                        for j in 0 .. (width - 1) / 2 do
                            (* | NW | NE |
                            |____|____|
                            | SW | SE | *)
                            // nw <- ne
                            res[i * width + j] <- img.Data[i * width + width - 1 - j]
                            // ne <- nw
                            res[i * width + width - 1 - j] <- img.Data[i * width + j]
                            // sw <- se
                            res[(height - 1) * width - (i * width) + j] <-
                                img.Data[(height - 1) * width - (i * width) + width - 1 - j]
                            // se <- sw
                            res[(height - 1) * width - (i * width) + width - 1 - j] <-
                                img.Data[(height - 1) * width - (i * width) + j]

                // Return the result
                res

            | EditType.Transformation filter ->
                let filterD = (Array.length filter) / 2

                let filter = Array.concat filter

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
                    // Weighted sum of pixels
                    Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

                // Array.mapi builds a new array, so we don't need to return anything explicitly here
                Array.mapi (fun i _ -> byte (processPixel i)) img.Data

        Image(res, width, height, img.Name)

    static member onGPU (clContext: ClContext) localWorkSize =

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
                    let filterD = (Array.length table) / 2
                    let filter = Array.concat table

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

module ImageProcessing.ImageProcessing

open System
open Brahma.FSharp
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

type RotationDirection =
    | Clockwise
    | Counterclockwise

type ReflectionDirection =
    | Horizontal
    | Vertical

[<RequireQualifiedAccess>]
type Kernel<'KernelFunction> =
    | Rotation of 'KernelFunction
    | Reflection of 'KernelFunction

    /// Returns kernel function wrapped in DU case
    static member makeRotationKernel (clContext: ClContext) localWorkSize direction =
        let kernel =
            match direction with
            | Clockwise ->
                <@
                    fun (r: Range1D) (img: ClArray<byte>) height width (result: ClArray<byte>) ->
                        let p = r.GlobalID0

                        if p < height * width then
                            let pi = p / width
                            let pj = p % width
                            result[pj * height + height - pi - 1] <- img[pi * width + pj]
                @>

            | Counterclockwise ->
                <@
                    fun (r: Range1D) (img: ClArray<byte>) height width (result: ClArray<byte>) ->
                        let p = r.GlobalID0

                        if p < height * width then
                            let pi = p / width
                            let pj = p % width
                            result[(width - pj - 1) * height + pi] <- img[pi * width + pj]
                @>

        let kernel = clContext.Compile kernel

        Rotation
        <| fun (commandQueue: MailboxProcessor<_>) (img: ClArray<byte>) height width (result: ClArray<byte>) ->

            let ndRange = Range1D.CreateValid(height * width, localWorkSize)

            let kernel = kernel.GetKernel()
            commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img height width result))
            commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
            result

    /// Returns kernel function wrapped in DU case
    static member makeReflectionKernel (clContext: ClContext) localWorkSize direction =
        let kernel =
            match direction with
            | Horizontal ->
                <@
                    fun (r: Range1D) (img: ClArray<byte>) height width (result: ClArray<byte>) ->
                        let p = r.GlobalID0

                        if p <= height * width / 2 then
                            let pi = p / width
                            let pj = p % width
                            result[pi * width + pj] <- img[(height - 1) * width - (pi * width) + pj]
                            result[(height - 1) * width - (pi * width) + pj] <- img[pi * width + pj]

                            result[pi * width + width - 1 - pj] <-
                                img[(height - 1) * width - (pi * width) + width - 1 - pj]

                            result[(height - 1) * width - (pi * width) + width - 1 - pj] <-
                                img[pi * width + width - 1 - pj]
                @>

            | Vertical ->
                <@
                    fun (r: Range1D) (img: ClArray<byte>) height width (result: ClArray<byte>) ->
                        let p = r.GlobalID0

                        if p <= height * width / 2 then
                            let pi = p / width
                            let pj = p % width
                            result[pi * width + pj] <- img[pi * width + width - 1 - pj]
                            result[pi * width + width - 1 - pj] <- img[pi * width + pj]

                            result[(height - 1) * width - (pi * width) + pj] <-
                                img[(height - 1) * width - (pi * width) + width - 1 - pj]

                            result[(height - 1) * width - (pi * width) + width - 1 - pj] <-
                                img[(height - 1) * width - (pi * width) + pj]
                @>

        let kernel = clContext.Compile kernel

        Reflection
        <| (fun (commandQueue: MailboxProcessor<_>) (img: ClArray<byte>) height width (result: ClArray<byte>) ->

            let ndRange = Range1D.CreateValid(height * width, localWorkSize)

            let kernel = kernel.GetKernel()
            commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img height width result))
            commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
            result)

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

let loadAsImage (file: string) =
    let img = Image.Load<L8> file

    let buf = Array.zeroCreate<byte> (img.Width * img.Height)

    img.CopyPixelDataTo(Span<byte> buf)
    Image(buf, img.Width, img.Height, System.IO.Path.GetFileName file)

let saveImage (image: Image) file =
    let img = Image.LoadPixelData<L8>(image.Data, image.Width, image.Height)
    img.Save file

let rotateCPU direction (img: Image) =
    let width = img.Width
    let height = img.Height
    let res = Array.zeroCreate (width * height)

    match direction with
    | Clockwise ->
        for i in 0 .. height - 1 do
            for j in 0 .. width - 1 do
                res[j * height + height - i - 1] <- img.Data[i * width + j]

    | Counterclockwise ->
        for i in 0 .. height - 1 do
            for j in 0 .. width - 1 do
                res[(width - j - 1) * height + i] <- img.Data[i * width + j]

    Image(res, height, width, img.Name)

let rawProcessGPU rawKernel (clContext: ClContext) =

    let kernel =
        match rawKernel with
        | Kernel.Rotation rotFn -> rotFn
        | Kernel.Reflection refFn -> refFn

    let queue = clContext.QueueProvider.CreateQueue()

    fun (img: Image) ->

        let input = clContext.CreateClArray<_>(img.Data, HostAccessMode.NotAccessible)

        let output =
            clContext.CreateClArray(
                img.Data.Length,
                HostAccessMode.NotAccessible,
                allocationMode = AllocationMode.Default
            )

        let (output: ClArray<byte>) = kernel queue input img.Height img.Width output

        let result = Array.zeroCreate (img.Height * img.Width)

        let result = queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(output, result, ch))
        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)

        match rawKernel with
        | Kernel.Rotation _ -> Image(result, img.Height, img.Width, img.Name)
        | Kernel.Reflection _ -> Image(result, img.Width, img.Height, img.Name)

let reflectCPU direction (img: Image) =
    let width = img.Width
    let height = img.Height
    let res = Array.zeroCreate (width * height)

    match direction with
    | Vertical ->
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

    | Horizontal ->
        for i in 0 .. (height - 1) / 2 do
            for j in 0 .. (width - 1) / 2 do
                // nw <- sw
                res[i * width + j] <- img.Data[(height - 1) * width - (i * width) + j]
                // sw <- nw
                res[(height - 1) * width - (i * width) + j] <- img.Data[i * width + j]
                // ne <- se
                res[i * width + width - 1 - j] <- img.Data[(height - 1) * width - (i * width) + width - 1 - j]
                // se <- ne
                res[(height - 1) * width - (i * width) + width - 1 - j] <- img.Data[i * width + width - 1 - j]

    Image(res, width, height, img.Name)

let gaussianBlurKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 4; 16; 24; 16; 4 |]
       [| 6; 24; 36; 24; 6 |]
       [| 4; 16; 24; 16; 4 |]
       [| 1; 4; 6; 4; 1 |] |]
    |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))

let edgesKernel =
    [| [| 0; 0; -1; 0; 0 |]
       [| 0; 0; -1; 0; 0 |]
       [| 0; 0; 2; 0; 0 |]
       [| 0; 0; 0; 0; 0 |]
       [| 0; 0; 0; 0; 0 |] |]
    |> Array.map (Array.map float32)

let laplacianKernel =
    [| [| -1; -3; -4; -3; -1 |]
       [| -3; 0; 6; 0; -3 |]
       [| -4; 6; 20; 6; -4 |]
       [| -3; 0; 6; 0; -3 |]
       [| -1; -3; -4; -3; -1 |] |]
    |> Array.map (Array.map float32)

let highPassKernel =
    [| [| -1; -1; -1; -1; -1 |]
       [| -1; -1; -1; -1; -1 |]
       [| -1; -1; 24; 1; 1 |]
       [| -1; -1; -1; -1; -1 |]
       [| -1; -1; -1; -1; -1 |] |]
    |> Array.map (Array.map float32)

let sobelVerticalKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 2; 8; 12; 8; 2 |]
       [| 0; 0; 0; 0; 0 |]
       [| -2; -8; -12; -8; -2 |]
       [| -1; -4; -6; -4; -1 |] |]
    |> Array.map (Array.map float32)

let applyFilterCPU (filter: float32[][]) (img: Image) =
    let height = img.Height
    let width = img.Width

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

        Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

    let data = Array.mapi (fun i _ -> byte (processPixel i)) img.Data
    Image(data, width, height, img.Name)

let applyFilterGPU kernel (clContext: ClContext) =

    let queue = clContext.QueueProvider.CreateQueue()

    fun (filter: float32[][]) (img: Image) ->

        let input = clContext.CreateClArray<_>(img.Data, HostAccessMode.NotAccessible)

        let output =
            clContext.CreateClArray(
                img.Data.Length,
                HostAccessMode.NotAccessible,
                allocationMode = AllocationMode.Default
            )

        let filterD = (Array.length filter) / 2

        let filter = Array.concat filter

        let clFilter =
            clContext.CreateClArray<_>(filter, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

        let (output: ClArray<byte>) =
            kernel queue clFilter filterD input img.Height img.Width output

        queue.Post(Msg.CreateFreeMsg clFilter)

        let result = Array.zeroCreate (img.Height * img.Width)

        let result = queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(output, result, ch))
        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)
        Image(result, img.Width, img.Height, img.Name)

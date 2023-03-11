module ImageProcessing.ImageProcessing

open System
open Brahma.FSharp
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

[<Struct>]
type Image =
    val Data: array<byte>
    val Width: int
    val Height: int
    val Name: string

    new(data, width, height, name) =
        { Data = data
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

let rotate90Clockwise (img: Image) =
    let width = img.Width
    let height = img.Height
    let result = Array.zeroCreate (height * width)

    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            result[j * height + height - i - 1] <- img.Data[i * width + j]

    Image(result, height, width, img.Name)

let rotate90Counterclockwise (img: Image) =
    let width = img.Width
    let height = img.Height
    let result = Array.zeroCreate (height * width)

    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            result[(width - j - 1) * height + i] <- img.Data[i * width + j]

    Image(result, height, width, img.Name)

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

let applyFilter (filter: float32[][]) (img: Image) =
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


let applyFilterGPUKernel (clContext: ClContext) localWorkSize =

    let kernel =
        <@
            fun (r: Range1D) (img: ClArray<_>) imgW imgH (filter: ClArray<_>) filterD (result: ClArray<_>) ->
                let p = r.GlobalID0
                let pw = p % imgW
                let ph = p / imgW
                let mutable res = 0.0f

                for i in ph - filterD .. ph + filterD do
                    for j in pw - filterD .. pw + filterD do
                        let mutable d = 0uy

                        if i < 0 || i >= imgH || j < 0 || j >= imgW then
                            d <- img[p]
                        else
                            d <- img[i * imgW + j]

                        let f = filter[(i - ph + filterD) * (2 * filterD + 1) + (j - pw + filterD)]
                        res <- res + (float32 d) * f

                result[p] <- byte (int res)
        @>

    let kernel = clContext.Compile kernel

    fun (commandQueue: MailboxProcessor<_>) (filter: ClArray<float32>) filterD (img: ClArray<byte>) imgH imgW (result: ClArray<_>) ->

        let ndRange = Range1D.CreateValid(imgH * imgW, localWorkSize)

        let kernel = kernel.GetKernel()
        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img imgW imgH filter filterD result))
        commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
        result

let applyFiltersGPU (clContext: ClContext) localWorkSize =
    let kernel = applyFilterGPUKernel clContext localWorkSize
    let queue = clContext.QueueProvider.CreateQueue()

    fun (filters: list<float32[][]>) (img: Image) ->

        let mutable input =
            clContext.CreateClArray<_>(img.Data, HostAccessMode.NotAccessible)

        let mutable output =
            clContext.CreateClArray(
                img.Data.Length,
                HostAccessMode.NotAccessible,
                allocationMode = AllocationMode.Default
            )

        for filter in filters do

            let filterD = (Array.length filter) / 2

            let filter = Array.concat filter

            let clFilter =
                clContext.CreateClArray<_>(filter, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

            let oldInput = input
            input <- kernel queue clFilter filterD input img.Height img.Width output
            output <- oldInput
            queue.Post(Msg.CreateFreeMsg clFilter)

        let result = Array.zeroCreate (img.Height * img.Width)

        let result = queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(input, result, ch))
        queue.Post(Msg.CreateFreeMsg input)
        queue.Post(Msg.CreateFreeMsg output)
        Image(result, img.Width, img.Height, img.Name)

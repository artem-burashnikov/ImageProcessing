module ImageProcessing.GPUKernel

open Brahma.FSharp

let rotation (clContext: ClContext) localWorkSize =
    let kernel =
        <@
            fun (r: Range1D) (img: ClArray<byte>) height width (result: ClArray<byte>) direction ->
                let p = r.GlobalID0
                let pi = p / width
                let pj = p % width

                if p < height * width then
                    if direction > 0 then
                        result[pj * height + height - pi - 1] <- img[pi * width + pj]
                    else
                        result[(width - pj - 1) * height + pi] <- img[pi * width + pj]
        @>

    let kernel = clContext.Compile kernel

    fun direction (commandQueue: MailboxProcessor<_>) (img: ClArray<byte>) height width (result: ClArray<byte>) ->

        let ndRange = Range1D.CreateValid(height * width, localWorkSize)

        let kernel = kernel.GetKernel()

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img height width result direction))

        commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
        result

let reflection (clContext: ClContext) localWorkSize =
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
                        result[pi * width + width - 1 - pj] <- img[(height - 1) * width - (pi * width) + width - 1 - pj]
                        result[(height - 1) * width - (pi * width) + width - 1 - pj] <- img[pi * width + width - 1 - pj]
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

        commandQueue.Post(Msg.MsgSetArguments(fun () -> kernel.KernelFunc ndRange img height width result direction))

        commandQueue.Post(Msg.CreateRunMsg<_, _> kernel)
        result

let filter (clContext: ClContext) localWorkSize =
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

module ImageProcessing.Streaming

open ImageProcessing.ImageProcessing

let listAllFiles dir = System.IO.Directory.GetFiles dir

type msg =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let imgSaver outDir =
    let outFile (imgName: string) = System.IO.Path.Combine(outDir, imgName)

    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    printfn "Image saver is finished!"
                    ch.Reply()
                | Img img ->
                    printfn $"Save: %A{img.Name}"
                    saveImage img (outFile img.Name)
                    return! loop ()
            }

        loop ())

let imgProcessor (transformation: Image -> Image) (imgSaver: MailboxProcessor<_>) =

    MailboxProcessor.Start(fun inbox ->
        let rec loop () =
            async {
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    printfn "Image processor is ready to finish!"
                    imgSaver.PostAndReply(EOS)
                    printfn "Image processor is finished!"
                    ch.Reply()
                | Img img ->
                    printfn $"Filter: %A{img.Name}"
                    let transformedImg = transformation img
                    imgSaver.Post(Img transformedImg)
                    return! loop ()
            }

        loop ())

let processAllFilesAgents1 (files: string seq) outDir transformations =

    let transformation = List.reduce (>>) transformations

    // Get the number of available logical processors on a machine
    let numCores = System.Environment.ProcessorCount

    let applicators =
        [| for _ in 1..numCores do
               yield imgProcessor transformation (imgSaver outDir) |]

    for file in files do
        (applicators |> Array.minBy (fun p -> p.CurrentQueueLength))
            .Post(Img(loadAsImage file))


    for applicator in applicators do
        applicator.PostAndReply(EOS)

let processAllFilesNaiveCPU (files: string seq) outDir transformations =

    for file in files do
        let img = loadAsImage file
        let output = List.fold (fun img filter -> filter img) img transformations
        saveImage output (System.IO.Path.Combine(outDir, img.Name))

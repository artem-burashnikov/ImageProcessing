module ImageProcessing.Streaming

open ImageProcessing.ImageProcessing
open RunStrategy

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
                    printfn "Saver is finished!"
                    ch.Reply()
                | Img img ->
                    printfn $"Saving: %A{img.Name}"
                    saveImage img (outFile img.Name)
                    printfn $"Saved: %A{img.Name}"
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
                    printfn $"Filtering: %A{img.Name}"
                    let transformedImg = transformation img
                    printfn $"Filtered: %A{img.Name}"
                    imgSaver.Post(Img transformedImg)
                    printfn $"Sent: %A{img.Name} to saver"
                    return! loop ()
            }

        loop ())

let processAllFilesAgentsCPU (runStrategy: RunStrategy) (files: string seq) outDir transformations =

    let transformation = List.reduce (>>) transformations

    let workers =
        match runStrategy with
        | Async1CPU -> [| imgProcessor transformation (imgSaver outDir) |]
        | _ -> failwith $"{runStrategy} is not yet implemented"

    // Start time it ...
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    for file in files do
        (workers |> Array.minBy (fun p -> p.CurrentQueueLength))
            .Post(Img(loadAsImage file))

    for worker in workers do
        worker.PostAndReply(EOS)

    // ... stop time it
    stopwatch.Stop()
    printfn $"%A{stopwatch.Elapsed.TotalMilliseconds}"

let processAllFilesNaiveCPU (files: string seq) outDir transformations =

    // Start time it ...
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    for file in files do
        let img = loadAsImage file
        printfn $"Filtering: %A{img.Name}"
        let output = List.fold (fun img filter -> filter img) img transformations
        printfn $"Saving: %A{img.Name}"
        saveImage output (System.IO.Path.Combine(outDir, img.Name))

    // ... stop time it
    stopwatch.Stop()
    printfn $"%A{stopwatch.Elapsed.TotalMilliseconds}"

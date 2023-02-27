module ImageProcessing.Streaming

open System.Collections
open ImageProcessing.ImageProcessing

let listAllFiles dir =
    System.IO.Directory.GetFiles dir

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

let imgProcessor filterApplicator (imgSaver: MailboxProcessor<_>) =

    let filter = filterApplicator

    MailboxProcessor.Start(fun inbox ->
        let rec loop cnt =
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
                    let filtered = filter img
                    imgSaver.Post(Img filtered)
                    return! loop (not cnt)
            }

        loop true)

let processAllFiles inDir outDir filterApplicators =
    let mutable cnt = 0

    let imgProcessors =
        filterApplicators
        |> List.map (fun x ->
            let imgSaver = imgSaver outDir
            imgProcessor x imgSaver)
        |> Array.ofList

    let filesToProcess = listAllFiles inDir

    for file in filesToProcess do
        (imgProcessors |> Array.minBy (fun p -> p.CurrentQueueLength))
            .Post(Img(loadAsImage file))

        cnt <- cnt + 1

    for imgProcessor in imgProcessors do
        imgProcessor.PostAndReply(EOS)

let processAllFilesNaiveCPU (files: string seq) outDir filterApplicators =

    for file in files do
        let ImgName = System.IO.Path.GetFileName file
        let img = loadAs2DArray file
        let output = List.fold (fun img filter -> filter img) img filterApplicators
        save2DByteArrayAsImage output (System.IO.Path.Combine(outDir, ImgName))

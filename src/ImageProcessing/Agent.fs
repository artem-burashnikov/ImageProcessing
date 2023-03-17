module ImageProcessing.Agent

open ImageProcessing.ImageProcessing

type Message =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let outFile outDir (imgName: string) = System.IO.Path.Combine(outDir, imgName)

type ImageAgent() =
    static let processorAndSaver id (transformation: Image -> Image) outDir =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop id =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            printfn $"Ending %A{id}"
                            ch.Reply()
                        | Img img ->
                            printfn $"Filtering %A{img.Name}"
                            let transformedImg = transformation img
                            printfn $"Saving %A{img.Name}"
                            saveImage transformedImg (outFile outDir img.Name)
                            return! loop id
                    }

                loop id)

    static let processor (transformation: Image -> Image) (imgSaver: MailboxProcessor<_>) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            printfn "Ending processor"
                            imgSaver.PostAndReply(EOS)
                            ch.Reply()
                        | Img img ->
                            printfn $"Filtering %A{img.Name}"
                            let transformedImg = transformation img
                            imgSaver.Post(Img transformedImg)
                            return! loop ()
                    }

                loop ())

    static let saver outDir =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            printfn "Ending saver"
                            ch.Reply()
                        | Img img ->
                            printfn $"Saving %A{img.Name}"
                            saveImage img (outFile outDir img.Name)
                            return! loop ()
                    }

                loop ())

    static member startProcessorAndSaver id transformation outDir =
        processorAndSaver id transformation outDir

    static member startProcessor transformation imgSaver = processor transformation imgSaver
    static member startSaver outDir = saver outDir

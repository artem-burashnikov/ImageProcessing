module ImageProcessing.Agent

open ImageProcessing.ImageProcessing

type Message =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let outFile outDir (imgName: string) = System.IO.Path.Combine(outDir, imgName)

type Agent() =
    static let superAgent id (transformation: Image -> Image) outDir =

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

    static let imgProcessor (transformation: Image -> Image) (imgSaver: MailboxProcessor<_>) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            imgSaver.PostAndReply(EOS)
                            ch.Reply()
                        | Img img ->
                            let transformedImg = transformation img
                            imgSaver.Post(Img transformedImg)
                            return! loop ()
                    }

                loop ())

    static let imgSaver outDir =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch -> ch.Reply()
                        | Img img ->
                            saveImage img (outFile outDir img.Name)
                            return! loop ()
                    }

                loop ())

    static member startSuperAgent id transformation outDir = superAgent id transformation outDir
    static member startImageProcessor transformation imgSaver = imgProcessor transformation imgSaver
    static member startImageSaver outDir = imgSaver outDir

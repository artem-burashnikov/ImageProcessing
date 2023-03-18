module ImageProcessing.Agent

open ImageProcessing.ImageProcessing
open ImageProcessing.Logging

type Message =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let getTime () =
    let now = System.DateTime.Now
    sprintf $"%02d{now.Hour}:%02d{now.Minute}:%02d{now.Second}:%03d{now.Millisecond}"

let outFile outDir (imgName: string) = System.IO.Path.Combine(outDir, imgName)

type ImageAgent() =
    static let processorAndSaver id (transformation: Image -> Image) outDir (logger: MailboxProcessor<_>) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            Logger.finishStatus (getTime ()) "Agent" "finished" id logger
                            ch.Reply()
                        | Img img ->
                            Logger.currentWork (getTime ()) img.Name "Agent" "processed" id logger
                            let transformedImg = transformation img
                            Logger.currentWork (getTime ()) img.Name "Agent" "saved" id logger
                            saveImage transformedImg (outFile outDir img.Name)
                            Logger.saveStatus (getTime ()) img.Name logger
                            return! loop ()
                    }

                loop ())

    static let processor
        id
        (transformation: Image -> Image)
        (imgSaver: MailboxProcessor<_>)
        (logger: MailboxProcessor<_>)
        =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            Logger.finishStatus (getTime ()) "ProcessorAgent" "ready to finish" id logger
                            imgSaver.PostAndReply(EOS)
                            Logger.finishStatus (getTime ()) "ProcessorAgent" "finished" id logger
                            ch.Reply()
                        | Img img ->
                            Logger.currentWork (getTime ()) img.Name "ProcessingAgent" "processed" id logger
                            let transformedImg = transformation img
                            imgSaver.Post(Img transformedImg)
                            return! loop ()
                    }

                loop ())

    static let saver id outDir (logger: MailboxProcessor<_>) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            Logger.finishStatus (getTime ()) "SavingAgent" "ready to finish" id logger
                            ch.Reply()
                        | Img img ->
                            Logger.currentWork (getTime ()) img.Name "SavingAgent" "saved" id logger
                            saveImage img (outFile outDir img.Name)
                            Logger.saveStatus (getTime ()) img.Name logger
                            return! loop ()
                    }

                loop ())

    static let logger = Logger.start ()

    static member startProcessorAndSaver id transformation outDir =
        processorAndSaver id transformation outDir logger

    static member startProcessor id transformation imgSaver =
        processor id transformation imgSaver logger

    static member startSaver id outDir = saver id outDir logger

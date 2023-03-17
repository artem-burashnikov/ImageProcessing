module ImageProcessing.Agent

open ImageProcessing.ImageProcessing

type Message =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let getTime () =
    let now = System.DateTime.Now
    sprintf $"%02d{now.Hour}:%02d{now.Minute}:%02d{now.Second}:%03d{now.Millisecond}"

let outFile outDir (imgName: string) = System.IO.Path.Combine(outDir, imgName)

type Logger() =

    static let logger =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () =
                async {
                    let! msg = inbox.Receive()
                    printfn $"{msg}"
                    return! loop ()
                }

            loop ())

    static member startLogger() = logger

type ImageAgent() =
    static let processorAndSaver id (transformation: Image -> Image) outDir (logger: MailboxProcessor<_>) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            let logMessage =
                                $"{getTime ()} : Agent#%d{id} has finished processing and saving images"

                            logger.Post(logMessage)
                            ch.Reply()
                        | Img img ->
                            let logMessage = $"{getTime ()} : %s{img.Name} is being processed by Agent#%d{id}"
                            logger.Post(logMessage)
                            let transformedImg = transformation img
                            let logMessage = $"{getTime ()} : %s{img.Name} is being saved by Agent#%d{id}"
                            saveImage transformedImg (outFile outDir img.Name)
                            logger.Post(logMessage)
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
                            let logMessage = $"{getTime ()} : ProcessorAgent#%d{id} is ready to finish"
                            logger.Post(logMessage)
                            imgSaver.PostAndReply(EOS)
                            let logMessage = $"{getTime ()} : ProcessorAgent#%d{id} is finished"
                            logger.Post(logMessage)
                            ch.Reply()
                        | Img img ->
                            let logMessage =
                                $"{getTime ()} : %s{img.Name} is being processed by ProcessorAgent#%d{id}"

                            logger.Post(logMessage)
                            let transformedImg = transformation img
                            let logMessage = $"{getTime ()} : %s{img.Name} is ready to be saved"
                            logger.Post(logMessage)
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
                            let logMessage = $"{getTime ()} : SavingAgent#%d{id} is finished"
                            logger.Post(logMessage)
                            ch.Reply()
                        | Img img ->
                            let logMessage = $"{getTime ()} : %s{img.Name} is being saved by SavingAgent#%d{id}"
                            logger.Post(logMessage)
                            saveImage img (outFile outDir img.Name)
                            let logMessage = $"{getTime ()} : %s{img.Name} has been saved"
                            logger.Post(logMessage)
                            return! loop ()
                    }

                loop ())

    static let logger = Logger.startLogger ()

    static member startProcessorAndSaver id transformation outDir =
        processorAndSaver id transformation outDir logger

    static member startProcessor id transformation imgSaver =
        processor id transformation imgSaver logger

    static member startSaver id outDir = saver id outDir logger

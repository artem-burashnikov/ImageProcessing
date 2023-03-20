module ImageProcessing.Agent

open ImageProcessing.ImageProcessing
open ImageProcessing.Logging

type Message =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let outFile outDir (imgName: string) = System.IO.Path.Combine(outDir, imgName)

(*
type ImageAgent() =
    static let processorAndSaver id (transformation: Image -> Image) outDir (logger: Logger) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            logger.Log $"%s{(getTime ())} : Agent#%d{id} is finished"
                            ch.Reply()
                        | Img img ->
                            logger.Log $"%s{(getTime ())} : %s{img.Name} is being processed by Agent#%d{id}"
                            let transformedImg = transformation img
                            logger.Log $"%s{(getTime ())} : %s{img.Name} is being saved by Agent#%d{id}"
                            saveImage transformedImg (outFile outDir img.Name)
                            logger.Log $"%s{(getTime ())} : %s{img.Name} has been saved successfully by Agent#%d{id}"
                            return! loop ()
                    }

                loop ())

    static let processor id (transformation: Image -> Image) (imgSaver: MailboxProcessor<_>) (logger: Logger) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            logger.Log $"%s{(getTime ())} : ProcessorAgent#%d{id} is ready to finish"
                            imgSaver.PostAndReply(EOS)
                            logger.Log $"%s{(getTime ())} : ProcessorAgent#%d{id} is finished"
                            ch.Reply()
                        | Img img ->
                            logger.Log $"%s{(getTime ())} : %s{img.Name} is being processed by ProcessorAgent#%d{id}"
                            let transformedImg = transformation img
                            imgSaver.Post(Img transformedImg)
                            return! loop ()
                    }

                loop ())

    static let saver id outDir (logger: Logger) =

        MailboxProcessor<Message>.Start
            (fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()

                        match msg with
                        | EOS ch ->
                            logger.Log $"%s{(getTime ())} : SavingAgent#%d{id} is ready to finish"
                            ch.Reply()
                        | Img img ->
                            logger.Log $"%s{(getTime ())} : %s{img.Name} is being saved by SavingAgent#%d{id}"
                            saveImage img (outFile outDir img.Name)

                            logger.Log
                                $"%s{(getTime ())} : %s{img.Name} has been saved successfully by SavingAgent#%d{id}"

                            return! loop ()
                    }

                loop ())

    static member startProcessorAndSaver id transformation outDir logger =
        processorAndSaver id transformation outDir logger

    static member startProcessor id transformation imgSaver logger =
        processor id transformation imgSaver logger

    static member startSaver id outDir logger = saver id outDir logger
*)

type ProcessorAndSaver(id: int, transformation: Image -> Image, outDir: string, logger: Logger) =

    let agent (inbox: MailboxProcessor<Message>) =
        let rec loop () =
            async {
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    logger.Log $"%s{(getTime ())}: Agent #%d{id} is finished"
                    ch.Reply()
                | Img img ->
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being processed by Agent #%d{id}"
                    let transformedImg = transformation img
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being saved by Agent #%d{id}"
                    saveImage transformedImg (outFile outDir img.Name)
                    logger.Log $"%s{(getTime ())}: %s{img.Name} has been saved successfully by Agent #%d{id}"
                    return! loop ()
            }

        loop ()

    let processorAndSaver = new MailboxProcessor<Message>(agent)
    member _.Start() = processorAndSaver.Start()
    member _.Stop() = processorAndSaver.PostAndReply(EOS)
    member _.ProcessAndSave img = processorAndSaver.Post(Img img)

type Saver(id: int, outDir: string, logger: Logger) =

    let agent (inbox: MailboxProcessor<Message>) =
        let rec loop () =
            async {
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    logger.Log $"%s{(getTime ())}: SavingAgent #%d{id} is ready to finish"
                    ch.Reply()
                | Img img ->
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being saved by SavingAgent #%d{id}"
                    saveImage img (outFile outDir img.Name)
                    logger.Log $"%s{(getTime ())}: %s{img.Name} has been saved successfully by SavingAgent #%d{id}"
                    return! loop ()
            }

        loop ()

    let saver = new MailboxProcessor<Message>(agent)
    member _.Start() = saver.Start()
    member _.Stop() = saver.PostAndReply(EOS)
    member _.Save img = saver.Post(Img img)

type Processor(id: int, transformation: Image -> Image, imgSaver: Saver, logger: Logger) =

    let agent (inbox: MailboxProcessor<Message>) =
        let rec loop () =
            async {
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    logger.Log $"%s{(getTime ())}: ProcessorAgent #%d{id} is ready to finish"
                    imgSaver.Stop()
                    logger.Log $"%s{(getTime ())}: ProcessorAgent #%d{id} is finished"
                    ch.Reply()
                | Img img ->
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being processed by ProcessorAgent #%d{id}"
                    let transformedImg = transformation img
                    imgSaver.Save transformedImg
                    return! loop ()
            }

        loop ()

    let processor = new MailboxProcessor<Message>(agent)
    member _.Start() = processor.Start()
    member _.Stop() = processor.PostAndReply(EOS)
    member _.Process img = processor.Post(Img img)

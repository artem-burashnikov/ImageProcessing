module ImageProcessing.Agent

open ImageProcessing.ImageProcessing
open ImageProcessing.Logging

type Message =
    | Img of Image
    | EOS of AsyncReplyChannel<unit>

let outFile outDir (imgName: string) = System.IO.Path.Combine(outDir, imgName)

type ProcessorAndSaver(id: int, transformation: Image -> Image, outDir: string, logger: Logger) =
    let mutable agentIsRunning = true

    let agent (inbox: MailboxProcessor<Message>) =
        async {
            while agentIsRunning do
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    logger.Log $"%s{(getTime ())}: Agent #%d{id} is finished"
                    agentIsRunning <- false
                    ch.Reply()
                | Img img ->
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being processed by Agent #%d{id}"
                    let transformedImg = transformation img
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being saved by Agent #%d{id}"
                    saveImage transformedImg (outFile outDir img.Name)
                    logger.Log $"%s{(getTime ())}: %s{img.Name} has been saved successfully by Agent #%d{id}"
        }

    let processorAndSaver = new MailboxProcessor<Message>(agent)
    member _.Start() = processorAndSaver.Start()
    member _.Stop() = processorAndSaver.PostAndReply(EOS)
    member _.ProcessAndSave img = processorAndSaver.Post(Img img)

type Saver(id: int, outDir: string, logger: Logger) =
    let mutable agentIsRunning = true

    let agent (inbox: MailboxProcessor<Message>) =
        async {
            while agentIsRunning do
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    logger.Log $"%s{(getTime ())}: SavingAgent #%d{id} is ready to finish"
                    agentIsRunning <- false
                    ch.Reply()
                | Img img ->
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being saved by SavingAgent #%d{id}"
                    saveImage img (outFile outDir img.Name)
                    logger.Log $"%s{(getTime ())}: %s{img.Name} has been saved successfully by SavingAgent #%d{id}"
        }

    let saver = new MailboxProcessor<Message>(agent)
    member _.Start() = saver.Start()
    member _.Stop() = saver.PostAndReply(EOS)
    member _.Save img = saver.Post(Img img)

type Processor(id: int, transformation: Image -> Image, imgSaver: Saver, logger: Logger) =
    let mutable agentIsRunning = true

    let agent (inbox: MailboxProcessor<Message>) =
        async {
            while agentIsRunning do
                let! msg = inbox.Receive()

                match msg with
                | EOS ch ->
                    logger.Log $"%s{(getTime ())}: ProcessorAgent #%d{id} is ready to finish"
                    imgSaver.Stop()
                    logger.Log $"%s{(getTime ())}: ProcessorAgent #%d{id} is finished"
                    agentIsRunning <- false
                    ch.Reply()
                | Img img ->
                    logger.Log $"%s{(getTime ())}: %s{img.Name} is being processed by ProcessorAgent #%d{id}"
                    let transformedImg = transformation img
                    imgSaver.Save transformedImg
        }

    let processor = new MailboxProcessor<Message>(agent)
    member _.Start() = processor.Start()
    member _.Stop() = processor.PostAndReply(EOS)
    member _.Process img = processor.Post(Img img)

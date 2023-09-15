/// <summary>
/// Contains types and functionality related to image processing agents.
/// </summary>
module ImageProcessing.Agent

open ImageProcessing.ImageProcessing.HelpProviders
open ImageProcessing.Logging

/// <summary>
/// Represents a message that can be sent to an agent.
/// </summary>
type Message =
    /// <summary>
    /// Represents an image to be processed.
    /// </summary>
    | Img of Image
    /// <summary>
    /// Represents the end of a message stream.
    /// </summary>
    | EOS of AsyncReplyChannel<unit>

/// <summary>
/// Gets the full output file path based on the output directory and image name.
/// </summary>
/// <param name="outDir">The output directory where the image will be saved.</param>
/// <param name="imgName">The name of the image.</param>
/// <returns>The full output file path.</returns>
let outFile (outDir: string) (imgName: string) : string = System.IO.Path.Combine(outDir, imgName)

/// <summary>
/// Represents an image processing and saving agent.
/// </summary>
/// <param name="id">The identifier for the agent.</param>
/// <param name="transformation">The function that defines the image transformation process.</param>
/// <param name="outDir">The output directory where processed images will be saved.</param>
/// <param name="logger">The logger used for logging agent activities.</param>
/// <returns>An instance of the ProcessorAndSaver class.</returns>
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

    /// <summary>
    /// Starts the agent for processing and saving images.
    /// </summary>
    member _.Start() = processorAndSaver.Start()

    /// <summary>
    /// Stops the agent gracefully.
    /// </summary>
    member _.Stop() = processorAndSaver.PostAndReply(EOS)

    /// <summary>
    /// Posts an image for processing and saving by the agent.
    /// </summary>
    /// <param name="img">The image to process and save.</param>
    member _.ProcessAndSave(img: Image) = processorAndSaver.Post(Img img)


/// <summary>
/// Represents an agent responsible for saving images.
/// </summary>
/// <param name="id">The identifier for the agent.</param>
/// <param name="outDir">The output directory where images will be saved.</param>
/// <param name="logger">The logger used for logging agent activities.</param>
type Saver(id: int, outDir: string, logger: Logger) =
    /// <summary>
    /// Gets a value indicating whether the agent is running.
    /// </summary>
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

    /// <summary>
    /// Starts the image-saving agent.
    /// </summary>
    member _.Start() = saver.Start()

    /// <summary>
    /// Stops the image-saving agent gracefully.
    /// </summary>
    member _.Stop() = saver.PostAndReply(EOS)

    /// <summary>
    /// Posts an image to be saved by the agent.
    /// </summary>
    /// <param name="img">The image to save.</param>
    member _.Save(img: Image) = saver.Post(Img img)


/// <summary>
/// Represents an agent responsible for processing images and using an image saver.
/// </summary>
/// <param name="id">The identifier for the agent.</param>
/// <param name="transformation">The function that defines the image transformation process.</param>
/// <param name="imgSaver">The image saver agent to use for saving processed images.</param>
/// <param name="logger">The logger used for logging agent activities.</param>
type Processor(id: int, transformation: Image -> Image, imgSaver: Saver, logger: Logger) =
    /// <summary>
    /// Gets a value indicating whether the agent is running.
    /// </summary>
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

    /// <summary>
    /// Starts the image processing agent.
    /// </summary>
    member _.Start() = processor.Start()

    /// <summary>
    /// Stops the image processing agent gracefully.
    /// </summary>
    member _.Stop() = processor.PostAndReply(EOS)

    /// <summary>
    /// Posts an image to be processed by the agent.
    /// </summary>
    /// <param name="img">The image to process.</param>
    member _.Process(img: Image) = processor.Post(Img img)

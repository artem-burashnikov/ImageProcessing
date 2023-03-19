module ImageProcessing.Streaming

open Brahma.FSharp
open ImageProcessing.Agent
open ImageProcessing.ImageProcessing
open ImageProcessing.Transformation
open ImageProcessing.RunStrategy
open Logging

let listAllFiles dir = System.IO.Directory.GetFiles dir

let processAllFiles (runStrategy: RunStrategy) (files: string seq) outDir transformations =

    /// Edit all image files one after another on the main thread.
    let naive (files: string seq) outDir transformations =
        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        // Iteratively process all files
        for file in files do
            let img = loadAsImage file
            Logger.log $"%s{getTime ()}: %s{img.Name} is loaded  for processing"

            Logger.log $"%s{getTime ()}: %s{img.Name} is being processed"
            let output = transformations img

            Logger.log $"%s{getTime ()}: %s{img.Name} is being saved"
            saveImage output (outFile outDir img.Name)
            Logger.log $"%s{(getTime ())} : %s{img.Name} has been saved successfully"

        // ... stop time it
        stopwatch.Stop()
        printfn $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"

    /// Edit image files utilizing agents pipeline.
    /// Processing and saving is divided between different agents.
    let async1 (files: string seq) outDir transformations =
        // Start agents for processing and saving images
        let imgSaver = ImageAgent.startSaver 1 outDir
        let agent = ImageAgent.startProcessor 1 transformations imgSaver

        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        // Process all files using previously started agents
        Seq.iter (fun file -> Img(loadAsImage file) |> agent.Post) files

        // Terminate agent after processing
        agent.PostAndReply(EOS)

        // ... end time it
        stopwatch.Stop()
        printfn $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"

    /// Edit images files dividing them between agents.
    /// Each agent processes and saves image by itself.
    let async2 (files: string seq) outDir transformations =
        // Get optimal parameters for parallel computations
        let numCores = System.Environment.ProcessorCount
        let numFiles = Seq.length files
        let numAgents = min numCores numFiles

        let splitWork =
            // For each file ...
            files
            // ... load it as an Image and pack it as a Message
            |> Seq.map (fun file -> loadAsImage file |> Img)
            // Then split all elements into optimal number of arrays
            |> Seq.splitInto numAgents

        // Start agents
        let agents =
            Array.init numAgents (fun id -> ImageAgent.startProcessorAndSaver (id + 1) transformations outDir)

        // Queue jobs
        Seq.iteri (fun i -> Array.iter agents[i].Post) splitWork

        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        // Terminate agents
        Seq.iter (fun (agent: MailboxProcessor<_>) -> agent.PostAndReply EOS) agents

        // ... end time it
        stopwatch.Stop()
        printfn $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"

    // Check that GPU is present on the system. If not, then force CPU run.
    let noGPU = Seq.isEmpty (ClDevice.GetAvailableDevices())

    let ensuredRunStrategy =
        if noGPU then
            match runStrategy with
            | GPU -> CPU
            | Async1GPU -> Async1CPU
            | Async2GPU -> Async2CPU
            | _ -> failwith "We only force CPU runs if no GPU is present on a device"
        else
            runStrategy

    // Determine how to run
    match ensuredRunStrategy with
    | CPU ->
        let transformations = transformations |> List.map getTsfCPU |> List.reduce (>>)
        naive files outDir transformations

    | Async1CPU ->
        let transformations = transformations |> List.map getTsfCPU |> List.reduce (>>)
        async1 files outDir transformations

    | Async2CPU ->
        let transformations = transformations |> List.map getTsfCPU |> List.reduce (>>)
        async2 files outDir transformations

    | GPU ->
        // At this point it has to be ensured that GPU is present on the system.
        let device = ClDevice.GetFirstAppropriateDevice()
        let context = ClContext(device)

        let transformations =
            transformations |> List.map (getTsfGPU context 64) |> List.reduce (>>)

        naive files outDir transformations

    | Async1GPU ->
        let device = ClDevice.GetFirstAppropriateDevice()
        let context = ClContext(device)

        let transformations =
            transformations |> List.map (getTsfGPU context 64) |> List.reduce (>>)

        async1 files outDir transformations

    | Async2GPU ->
        let device = ClDevice.GetFirstAppropriateDevice()
        let context = ClContext(device)

        let transformations =
            transformations |> List.map (getTsfGPU context 64) |> List.reduce (>>)

        async2 files outDir transformations

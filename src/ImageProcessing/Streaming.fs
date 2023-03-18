module ImageProcessing.Streaming

open Brahma.FSharp
open ImageProcessing.Agent
open ImageProcessing.ImageProcessing
open ImageProcessing.Transformation
open ImageProcessing.RunStrategy
open Logging

let listAllFiles dir = System.IO.Directory.GetFiles dir

let processAllFiles (runStrategy: RunStrategy) (files: string seq) outDir transformations =

    let naive (files: string seq) outDir transformations =
        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        let logger = Logger.start ()

        // Iteratively process all files
        for file in files do
            let img = loadAsImage file
            Logger.general (sprintf $"%A{getTime ()}: Loaded %s{img.Name} for processing") logger
            let output = transformations img
            Logger.general (sprintf $"%A{getTime ()}: %s{img.Name} was processed") logger
            Logger.general (sprintf $"%A{getTime ()}: %s{img.Name} is being saved") logger
            saveImage output (outFile outDir img.Name)
            Logger.general (sprintf $"%A{getTime ()}: %s{img.Name} has been saved") logger

        // ... stop time it
        stopwatch.Stop()
        printfn $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"

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

        // Queue jobs in parallel
        Seq.iteri (fun i -> Array.iter agents[i].Post) splitWork

        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        // Terminate agents
        Seq.iter (fun (agent: MailboxProcessor<_>) -> agent.PostAndReply EOS) agents

        // ... end time it
        stopwatch.Stop()
        printfn $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"

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

    match ensuredRunStrategy with
    | CPU ->
        let transformations =
            transformations |> List.map (getTsf applyFilter) |> List.reduce (>>)

        naive files outDir transformations
    | Async1CPU ->
        let transformations =
            transformations |> List.map (getTsf applyFilter) |> List.reduce (>>)

        async1 files outDir transformations
    | Async2CPU ->
        let transformations =
            transformations |> List.map (getTsf applyFilter) |> List.reduce (>>)

        async2 files outDir transformations
    | GPU ->
        let device = ClDevice.GetFirstAppropriateDevice()
        let context = ClContext(device)
        let applyTransformations = applyFilterGPU context 64

        let transformations =
            transformations |> List.map (getTsf applyTransformations) |> List.reduce (>>)

        naive files outDir transformations
    | Async1GPU ->
        let device = ClDevice.GetFirstAppropriateDevice()
        let context = ClContext(device)
        let applyTransformations = applyFilterGPU context 64

        let transformations =
            transformations |> List.map (getTsf applyTransformations) |> List.reduce (>>)

        async1 files outDir transformations
    | Async2GPU ->
        let device = ClDevice.GetFirstAppropriateDevice()
        let context = ClContext(device)
        let applyTransformations = applyFilterGPU context 64

        let transformations =
            transformations |> List.map (getTsf applyTransformations) |> List.reduce (>>)

        async2 files outDir transformations

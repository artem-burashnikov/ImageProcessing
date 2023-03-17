module ImageProcessing.Streaming

open ImageProcessing.Agent
open ImageProcessing.ImageProcessing
open ImageProcessing.Transformation
open ImageProcessing.RunStrategy

let listAllFiles dir = System.IO.Directory.GetFiles dir

/// Make a composition of a single transformation out of a list of transformations
let composeFinalCPUTsf transformations =
    transformations |> List.map getCPUTsf |> List.reduce (>>)

let processAllFiles (runStrategy: RunStrategy) (files: string seq) outDir transformations =

    match runStrategy with
    | CPU ->
        // Make a single composition out of every transformation
        let transform = composeFinalCPUTsf transformations

        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        // Iteratively process all files
        for file in files do
            let img = loadAsImage file
            let output = transform img
            saveImage output (outFile outDir img.Name)

        // ... stop time it
        stopwatch.Stop()
        printfn $"%A{stopwatch.Elapsed.TotalMilliseconds}"

    | Async1CPU ->
        // Make a single composition out of every transformation
        let transform = composeFinalCPUTsf transformations

        // Start agents for processing and saving images
        let imgSaver = ImageAgent.startSaver outDir
        let agent = ImageAgent.startProcessor transform imgSaver

        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        // Process all files using previously started agents
        Seq.iter (fun file -> Img(loadAsImage file) |> agent.Post) files

        // Terminate agent after processing
        agent.PostAndReply(EOS)

        // ... end time it
        stopwatch.Stop()
        printfn $"%A{stopwatch.Elapsed.TotalMilliseconds}"

    | Async2CPU ->
        // Make a single composition out of every transformation
        let transform = composeFinalCPUTsf transformations

        // Get optimal parameters for parallel computations
        let numCores = System.Environment.ProcessorCount
        let numFiles = Seq.length files
        let numAgents = min numCores numFiles

        let splitWork =
            // For each file ...
            files
            // ... load it as Image and pack it as a Message
            |> Seq.map (fun s -> Img(loadAsImage s))
            // Then split into optimal number of arrays
            |> Seq.splitInto numAgents
            |> Array.ofSeq

        // Start agents
        let agents =
            Array.init numAgents (fun id -> ImageAgent.startProcessorAndSaver (id + 1) transform outDir)

        // Queue jobs in parallel
        Array.Parallel.iteri (fun i -> Array.iter agents[i].Post) splitWork

        // Start time it ...
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        // Terminate agents
        Array.iter (fun (processor: MailboxProcessor<_>) -> processor.PostAndReply EOS) agents

        // ... end time it
        stopwatch.Stop()
        printfn $"%A{stopwatch.Elapsed.TotalMilliseconds}"

    | _ -> failwith $"{runStrategy} is not is not yet implemented"

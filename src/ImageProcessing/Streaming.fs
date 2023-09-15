/// <summary>
/// Provides functions for processing a sequence of image files using different run strategies.
/// </summary>
module ImageProcessing.Streaming

open System.Diagnostics
open ImageProcessing.Agent
open ImageProcessing.ImageProcessing.HelpProviders
open ImageProcessing.Transformation
open ImageProcessing.RunStrategy
open ImageProcessing.Logging

/// <summary>
/// Lists all files in a directory.
/// </summary>
/// <param name="dir">The directory path.</param>
/// <returns>A sequence of file paths.</returns>
let listAllFiles dir = System.IO.Directory.GetFiles dir

/// <summary>
/// Processes a sequence of image files using the specified run strategy and transformations.
/// </summary>
/// <param name="runStrategy">The run strategy to use.</param>
/// <param name="threads">The number of threads or agents to use for processing.</param>
/// <param name="files">The sequence of image file paths to process.</param>
/// <param name="outDir">The output directory for saving processed images.</param>
/// <param name="transformations">The image transformations to apply.</param>
let processAllFiles (runStrategy: RunStrategy) (threads: uint) (files: string seq) outDir transformations =

    // Fail-safe check
    if threads = 0u then
        failwith "Number of threads cannot be 0"

    // Initialize transformation instance
    let threads = System.Convert.ToInt32 threads

    // Start a new instance of logger
    let logger = Logger()
    logger.Start()

    // Ensure that the chosen run strategy is available
    let ensuredRunStrategy =
        if GPUDevice.noGPU () then
            logger.Log "GPGPU was not found on the system. Using CPU instead"
            switchToCPU runStrategy
        else
            runStrategy

    /// <summary>
    /// Processes image files one after another on the main thread.
    /// </summary>
    /// <param name="files">The sequence of image file paths to process.</param>
    /// <param name="outDir">The output directory for saving processed images.</param>
    /// <param name="transformations">The image transformations to apply.</param>
    /// Edit all image files one after another on the main thread.
    let naive (files: string seq) outDir transformations =
        // Start time it ...
        let stopwatch = Stopwatch.StartNew()

        // Iteratively process all files
        for file in files do
            let img = loadAsImage file
            logger.Log $"%s{getTime ()}: %s{img.Name} is loaded  for processing"

            logger.Log $"%s{getTime ()}: %s{img.Name} is being processed"
            let output = transformations img

            logger.Log $"%s{getTime ()}: %s{img.Name} is being saved"
            saveImage output (outFile outDir img.Name)
            logger.Log $"%s{(getTime ())}: %s{img.Name} has been saved successfully"

        // ... stop time it
        stopwatch.Stop()
        // Log the elapsed time
        logger.Log $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"
        // Stop the logger
        logger.Stop()

    /// <summary>
    /// Processes image files utilizing an agents pipeline.
    /// </summary>
    /// <param name="files">The sequence of image file paths to process.</param>
    /// <param name="outDir">The output directory for saving processed images.</param>
    /// <param name="transformations">The image transformations to apply.</param>
    let async1 (files: string seq) outDir transformations =
        // Start agents for processing and saving images
        let imgSaver = Saver(1, outDir, logger)
        imgSaver.Start()
        let imgProcessor = Processor(1, transformations, imgSaver, logger)
        imgProcessor.Start()

        // Start time it ...
        let stopwatch = Stopwatch.StartNew()

        // Process all files using previously started agents
        Seq.iter
            (fun file ->
                let img = loadAsImage file
                logger.Log $"%s{getTime ()}: %s{img.Name} is loaded  for processing"
                imgProcessor.Process img)
            files

        // Stop precessing agent. That will automatically stop saving agent.
        imgProcessor.Stop()

        // ... end time it
        stopwatch.Stop()
        // Log the elapsed time
        logger.Log $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"
        // Stop the logger
        logger.Stop()

    /// <summary>
    /// Processes image files dividing them between agents.
    /// </summary>
    /// <param name="files">The sequence of image file paths to process.</param>
    /// <param name="outDir">The output directory for saving processed images.</param>
    /// <param name="transformations">The image transformations to apply.</param>
    /// <param name="threads">The number of threads or agents to use for processing.</param>
    let async2 (files: string seq) outDir transformations threads =
        // Get optimal parameters for parallel computations
        let numCores = min threads System.Environment.ProcessorCount
        let numFiles = Seq.length files
        let numAgents = min numCores numFiles

        let splitWork =
            // Load files
            files
            |> Seq.map (fun file ->
                let img = loadAsImage file
                logger.Log $"%s{getTime ()}: %s{img.Name} is loaded  for processing"
                img)
            // Then split all loaded files into optimal number of arrays
            |> Seq.splitInto numAgents

        // Initialize agents
        let agents =
            Array.init numAgents (fun id -> ProcessorAndSaver(id + 1, transformations, outDir, logger))

        // Start agents and give them jobs
        Seq.iteri
            (fun i ->
                agents[ i ].Start()
                Array.iter agents[i].ProcessAndSave)
            splitWork

        // Start time it ...
        let stopwatch = Stopwatch.StartNew()

        // Terminate agents
        Seq.iter (fun (agent: ProcessorAndSaver) -> agent.Stop()) agents

        // ... end time it
        stopwatch.Stop()
        // Log the elapsed time
        logger.Log $"Total elapsed time: %02.0f{stopwatch.Elapsed.TotalMilliseconds} ms"
        // Stop the logger
        logger.Stop()

    // Determine how to run
    match ensuredRunStrategy with
    | CPU ->
        let transformations = transformationsOnCPU transformations threads
        naive files outDir transformations

    | Async1CPU ->
        let transformations = transformationsOnCPU transformations threads
        async1 files outDir transformations

    | Async2CPU ->
        let transformations = transformationsOnCPU transformations threads
        async2 files outDir transformations threads

    | GPU ->
        let transformations =
            transformationsOnGPU transformations GPUDevice.context GPUDevice.localWorkSize

        naive files outDir transformations

    | Async1GPU ->
        let transformations =
            transformationsOnGPU transformations GPUDevice.context GPUDevice.localWorkSize

        async1 files outDir transformations

    | Async2GPU ->
        let transformations =
            transformationsOnGPU transformations GPUDevice.context GPUDevice.localWorkSize

        async2 files outDir transformations threads

module ImageProcessing.RunStrategy

type RunStrategy =
    /// Naive image processing on CPU without workers
    | CPU
    /// Naive image processing on GPU without workers
    | GPU
    /// Image processing on CPU: Some workers process images, while others save them
    | Async1CPU
    /// Image processing on CPU: Each worker processes and saves images
    | Async2CPU
    /// Image processing on GPU: Some workers process images, while others save them
    | AsyncGPU

    override this.ToString() =
        match this with
        | CPU -> "CPU"
        | GPU -> "GPU"
        | Async1CPU -> "Async1CPU"
        | Async2CPU -> "Async2CPU"
        | AsyncGPU -> "AsyncGPU"

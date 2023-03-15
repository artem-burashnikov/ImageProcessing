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
    | Async1GPU
    /// Image processing on GPU: Each worker processes and saves images
    | Async2GPU

    override this.ToString() =
        match this with
        | CPU -> "CPU"
        | GPU -> "GPU"
        | Async1CPU -> "Async1CPU"
        | Async2CPU -> "Async2CPU"
        | Async1GPU -> "Async1GPU"
        | Async2GPU -> "Async2GPU"

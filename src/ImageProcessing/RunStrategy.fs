module ImageProcessing.RunStrategy

type RunStrategy =
    /// Naive image processing on CPU without workers
    | CPU
    /// Naive image processing on GPU without workers
    | GPU
    /// Image processing on CPU: pipeline of agents passing parts of completed work to each other
    | Async1CPU
    /// Image processing on CPU: images are split between agents who do all processing themselves
    | Async2CPU
    /// Image processing on GPU: pipeline of agents
    | Async1GPU
    /// Image processing on GPU: images are split between agents who do all processing themselves
    | Async2GPU

    override this.ToString() =
        match this with
        | CPU -> "CPU"
        | GPU -> "GPU"
        | Async1CPU -> "Async1CPU"
        | Async2CPU -> "Async2CPU"
        | Async1GPU -> "Async1GPU"
        | Async2GPU -> "Async2GPU"

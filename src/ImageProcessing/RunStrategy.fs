/// <summary>
/// Represents different run strategies for image processing.
/// </summary>
module ImageProcessing.RunStrategy

/// <summary>
/// Defines various run strategies for image processing.
/// </summary>
type RunStrategy =
    | CPU
    | GPU
    | Async1CPU
    | Async2CPU
    | Async1GPU
    | Async2GPU

/// <summary>
/// Switches to a CPU-based run strategy based on the input strategy.
/// </summary>
/// <param name="strategy">The original run strategy.</param>
/// <returns>The CPU-based run strategy.</returns>q
let switchToCPU =
    function
    | CPU -> CPU
    | GPU -> CPU
    | Async1CPU -> Async1CPU
    | Async2CPU -> Async2CPU
    | Async1GPU -> Async1CPU
    | Async2GPU -> Async2CPU

module ImageProcessing.RunStrategy

type RunStrategy =
    | CPU
    | GPU
    | Async1CPU
    | Async2CPU
    | Async1GPU
    | Async2GPU

let switchToCPU =
    function
    | CPU -> CPU
    | GPU -> CPU
    | Async1CPU -> Async1CPU
    | Async2CPU -> Async2CPU
    | Async1GPU -> Async1CPU
    | Async2GPU -> Async2CPU

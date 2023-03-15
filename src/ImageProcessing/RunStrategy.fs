module ImageProcessing.RunStrategy

type RunStrategy =
    | CPU
    | AsyncCPU1
    | AsyncCPU2
    | GPU

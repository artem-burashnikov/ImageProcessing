module ImageProcessing.FilterKernel

let gaussianBlurKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 4; 16; 24; 16; 4 |]
       [| 6; 24; 36; 24; 6 |]
       [| 4; 16; 24; 16; 4 |]
       [| 1; 4; 6; 4; 1 |] |]
    |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))
    |> array2D

let edgesKernel =
    [| [| 0; 0; -1; 0; 0 |]
       [| 0; 0; -1; 0; 0 |]
       [| 0; 0; 2; 0; 0 |]
       [| 0; 0; 0; 0; 0 |]
       [| 0; 0; 0; 0; 0 |] |]
    |> Array.map (Array.map float32)
    |> array2D

let laplacianKernel =
    [| [| -1; -3; -4; -3; -1 |]
       [| -3; 0; 6; 0; -3 |]
       [| -4; 6; 20; 6; -4 |]
       [| -3; 0; 6; 0; -3 |]
       [| -1; -3; -4; -3; -1 |] |]
    |> Array.map (Array.map float32)
    |> array2D

let highPassKernel =
    [| [| -1; -1; -1; -1; -1 |]
       [| -1; -1; -1; -1; -1 |]
       [| -1; -1; 24; 1; 1 |]
       [| -1; -1; -1; -1; -1 |]
       [| -1; -1; -1; -1; -1 |] |]
    |> Array.map (Array.map float32)
    |> array2D

let sobelVerticalKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 2; 8; 12; 8; 2 |]
       [| 0; 0; 0; 0; 0 |]
       [| -2; -8; -12; -8; -2 |]
       [| -1; -4; -6; -4; -1 |] |]
    |> Array.map (Array.map float32)
    |> array2D

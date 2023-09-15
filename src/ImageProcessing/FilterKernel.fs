/// <summary>
/// Provides predefined filter kernels for image processing operations.
/// </summary>
module ImageProcessing.FilterKernel

/// <summary>
/// Represents a Gaussian blur kernel for image smoothing.
/// </summary>
let gaussianBlurKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 4; 16; 24; 16; 4 |]
       [| 6; 24; 36; 24; 6 |]
       [| 4; 16; 24; 16; 4 |]
       [| 1; 4; 6; 4; 1 |] |]
    |> Array.map (Array.map (fun x -> (float32 x) / 256.0f))
    |> array2D

/// <summary>
/// Represents an edges detection kernel.
/// </summary>
let edgesKernel =
    [| [| 0; 0; -1; 0; 0 |]
       [| 0; 0; -1; 0; 0 |]
       [| 0; 0; 2; 0; 0 |]
       [| 0; 0; 0; 0; 0 |]
       [| 0; 0; 0; 0; 0 |] |]
    |> Array.map (Array.map float32)
    |> array2D

/// <summary>
/// Represents a Laplacian kernel for edge detection and sharpening.
/// </summary>
let laplacianKernel =
    [| [| -1; -3; -4; -3; -1 |]
       [| -3; 0; 6; 0; -3 |]
       [| -4; 6; 20; 6; -4 |]
       [| -3; 0; 6; 0; -3 |]
       [| -1; -3; -4; -3; -1 |] |]
    |> Array.map (Array.map float32)
    |> array2D

/// <summary>
/// Represents a high-pass filter kernel for emphasizing fine details.
/// </summary>
let highPassKernel =
    [| [| -1; -1; -1; -1; -1 |]
       [| -1; -1; -1; -1; -1 |]
       [| -1; -1; 24; 1; 1 |]
       [| -1; -1; -1; -1; -1 |]
       [| -1; -1; -1; -1; -1 |] |]
    |> Array.map (Array.map float32)
    |> array2D

/// <summary>
/// Represents a Sobel vertical edge detection kernel.
/// </summary>
let sobelVerticalKernel =
    [| [| 1; 4; 6; 4; 1 |]
       [| 2; 8; 12; 8; 2 |]
       [| 0; 0; 0; 0; 0 |]
       [| -2; -8; -12; -8; -2 |]
       [| -1; -4; -6; -4; -1 |] |]
    |> Array.map (Array.map float32)
    |> array2D

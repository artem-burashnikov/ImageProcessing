/// <summary>
/// Contains types and functions for applying various image transformations.
/// </summary>
module ImageProcessing.Transformation

open ImageProcessing.ImageProcessing
open ImageProcessing.HelpProviders
open ImageProcessing.FilterKernel
open Brahma.FSharp
open FSharp.Reflection

/// <summary>
/// Represents different image transformation types.
/// </summary>
type Transformation =
    | Blur
    | Edges
    | HighPass
    | Laplacian
    | SobelV
    | Rotate
    | RotateCCW
    | ReflectH
    | ReflectV

    /// <summary>
    /// Gets an array of all available image transformation types.
    /// </summary>
    static member all =
        let cases = FSharpType.GetUnionCases(typeof<Transformation>)

        [| for case in cases do
               let transformation = FSharpValue.MakeUnion(case, [||]) :?> Transformation
               yield transformation |]

/// <summary>
/// Returns a CPU-based image transformation function for a specific transformation type.
/// </summary>
/// <param name="threads">The number of threads to use for parallel processing.</param>
/// <param name="transformationType">The type of image transformation to apply.</param>
/// <returns>A function that can apply the specified transformation using CPU resources.</returns>
let getTsfCPU threads =
    function
    | Blur -> CPU.applyTransform threads (EditType.Transformation gaussianBlurKernel)
    | Edges -> CPU.applyTransform threads (EditType.Transformation edgesKernel)
    | HighPass -> CPU.applyTransform threads (EditType.Transformation highPassKernel)
    | Laplacian -> CPU.applyTransform threads (EditType.Transformation laplacianKernel)
    | SobelV -> CPU.applyTransform threads (EditType.Transformation sobelVerticalKernel)
    | Rotate -> CPU.applyTransform threads (EditType.Rotation Clockwise)
    | RotateCCW -> CPU.applyTransform threads (EditType.Rotation Counterclockwise)
    | ReflectH -> CPU.applyTransform threads (EditType.Reflection Horizontal)
    | ReflectV -> CPU.applyTransform threads (EditType.Reflection Vertical)

/// <summary>
/// Returns a GPU-based image transformation function for a specific transformation type.
/// </summary>
/// <param name="clContext">The OpenCL context for GPU operations.</param>
/// <param name="localWorkSize">The local work size for GPU parallelism.</param>
/// <param name="transformationType">The type of image transformation to apply.</param>
/// <returns>A function that can apply the specified transformation using GPU resources.</returns>
let getTsfGPU (clContext: ClContext) localWorkSize =
    function
    | Blur -> GPU.applyTransform clContext localWorkSize (EditType.Transformation gaussianBlurKernel)
    | Edges -> GPU.applyTransform clContext localWorkSize (EditType.Transformation edgesKernel)
    | HighPass -> GPU.applyTransform clContext localWorkSize (EditType.Transformation highPassKernel)
    | Laplacian -> GPU.applyTransform clContext localWorkSize (EditType.Transformation laplacianKernel)
    | SobelV -> GPU.applyTransform clContext localWorkSize (EditType.Transformation sobelVerticalKernel)
    | Rotate -> GPU.applyTransform clContext localWorkSize (EditType.Rotation Clockwise)
    | RotateCCW -> GPU.applyTransform clContext localWorkSize (EditType.Rotation Counterclockwise)
    | ReflectH -> GPU.applyTransform clContext localWorkSize (EditType.Reflection Horizontal)
    | ReflectV -> GPU.applyTransform clContext localWorkSize (EditType.Reflection Vertical)

/// <summary>
/// Applies a sequence of image transformations to an image using CPU resources.
/// </summary>
/// <param name="transformationsList">The list of image transformations to apply sequentially.</param>
/// <param name="threads">The number of threads to use for parallel processing.</param>
/// <returns>The transformed image.</returns>
let transformationsOnCPU transformationsList threads =
    transformationsList |> List.map (getTsfCPU threads) |> List.reduce (>>)

/// <summary>
/// Applies a sequence of image transformations to an image using GPU resources.
/// </summary>
/// <param name="transformationsList">The list of image transformations to apply sequentially.</param>
/// <param name="context">The OpenCL context for GPU operations.</param>
/// <param name="localWorkSize">The local work size for GPU parallelism.</param>
/// <returns>The transformed image.</returns>
let transformationsOnGPU transformationsList context localWorkSize =
    transformationsList
    |> List.map (getTsfGPU context localWorkSize)
    |> List.reduce (>>)

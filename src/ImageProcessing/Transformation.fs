module ImageProcessing.Transformation

open ImageProcessing.ImageProcessing
open ImageProcessing.HelpProviders
open ImageProcessing.FilterKernel
open Brahma.FSharp
open FSharp.Reflection

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

    static member all =
        let cases = FSharpType.GetUnionCases(typeof<Transformation>)

        [| for case in cases do
               let transformation = FSharpValue.MakeUnion(case, [||]) :?> Transformation
               yield transformation |]

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

let transformationsOnCPU transformationsList threads =
    transformationsList |> List.map (getTsfCPU threads) |> List.reduce (>>)

let transformationsOnGPU transformationsList context localWorkSize =
    transformationsList
    |> List.map (getTsfGPU context localWorkSize)
    |> List.reduce (>>)

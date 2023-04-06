module ImageProcessing.Transformation

open ImageProcessing.ImageProcessing
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

let getTsfCPU (transform: ApplyTransform) =
    function
    | Blur -> transform.OnCPU(EditType.Transformation gaussianBlurKernel)
    | Edges -> transform.OnCPU(EditType.Transformation edgesKernel)
    | HighPass -> transform.OnCPU(EditType.Transformation highPassKernel)
    | Laplacian -> transform.OnCPU(EditType.Transformation laplacianKernel)
    | SobelV -> transform.OnCPU(EditType.Transformation sobelVerticalKernel)
    | Rotate -> transform.OnCPU(EditType.Rotation Clockwise)
    | RotateCCW -> transform.OnCPU(EditType.Rotation Counterclockwise)
    | ReflectH -> transform.OnCPU(EditType.Reflection Horizontal)
    | ReflectV -> transform.OnCPU(EditType.Reflection Vertical)

let getTsfGPU (transform: ApplyTransform) (clContext: ClContext) localWorkSize =
    function
    | Blur -> transform.OnGPU clContext localWorkSize (EditType.Transformation gaussianBlurKernel)
    | Edges -> transform.OnGPU clContext localWorkSize (EditType.Transformation edgesKernel)
    | HighPass -> transform.OnGPU clContext localWorkSize (EditType.Transformation highPassKernel)
    | Laplacian -> transform.OnGPU clContext localWorkSize (EditType.Transformation laplacianKernel)
    | SobelV -> transform.OnGPU clContext localWorkSize (EditType.Transformation sobelVerticalKernel)
    | Rotate -> transform.OnGPU clContext localWorkSize (EditType.Rotation Clockwise)
    | RotateCCW -> transform.OnGPU clContext localWorkSize (EditType.Rotation Counterclockwise)
    | ReflectH -> transform.OnGPU clContext localWorkSize (EditType.Reflection Horizontal)
    | ReflectV -> transform.OnGPU clContext localWorkSize (EditType.Reflection Vertical)

let transformationsOnCPU transformationsList transform =
    transformationsList |> List.map (getTsfCPU transform) |> List.reduce (>>)

let transformationsOnGPU transformationsList transform context localWorkSize =
    transformationsList
    |> List.map (getTsfGPU transform context localWorkSize)
    |> List.reduce (>>)

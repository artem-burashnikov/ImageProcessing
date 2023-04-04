module ImageProcessing.Transformation

open ImageProcessing.ImageProcessing
open ImageProcessing.FilterKernel
open Brahma.FSharp

[<RequireQualifiedAccess>]
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
        [| Blur
           Edges
           HighPass
           Laplacian
           SobelV
           Rotate
           RotateCCW
           ReflectH
           ReflectV |]

let getTsfCPU (transform: ApplyTransform) transformation =
    match transformation with
    | Transformation.Blur -> transform.OnCPU(EditType.Transformation gaussianBlurKernel)
    | Transformation.Edges -> transform.OnCPU(EditType.Transformation edgesKernel)
    | Transformation.HighPass -> transform.OnCPU(EditType.Transformation highPassKernel)
    | Transformation.Laplacian -> transform.OnCPU(EditType.Transformation laplacianKernel)
    | Transformation.SobelV -> transform.OnCPU(EditType.Transformation sobelVerticalKernel)
    | Transformation.Rotate -> transform.OnCPU(EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> transform.OnCPU(EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> transform.OnCPU(EditType.Reflection Horizontal)
    | Transformation.ReflectV -> transform.OnCPU(EditType.Reflection Vertical)

let getTsfGPU (transform: ApplyTransform) (clContext: ClContext) localWorkSize transformation =
    match transformation with
    | Transformation.Blur -> transform.OnGPU clContext localWorkSize (EditType.Transformation gaussianBlurKernel)
    | Transformation.Edges -> transform.OnGPU clContext localWorkSize (EditType.Transformation edgesKernel)
    | Transformation.HighPass -> transform.OnGPU clContext localWorkSize (EditType.Transformation highPassKernel)
    | Transformation.Laplacian -> transform.OnGPU clContext localWorkSize (EditType.Transformation laplacianKernel)
    | Transformation.SobelV -> transform.OnGPU clContext localWorkSize (EditType.Transformation sobelVerticalKernel)
    | Transformation.Rotate -> transform.OnGPU clContext localWorkSize (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> transform.OnGPU clContext localWorkSize (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> transform.OnGPU clContext localWorkSize (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> transform.OnGPU clContext localWorkSize (EditType.Reflection Vertical)

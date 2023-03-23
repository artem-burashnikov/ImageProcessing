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

let getTsfCPU transformation =


    match transformation with
    | Transformation.Blur -> ApplyTransform.onCPU (EditType.Transformation FilterKernel.GaussianBlurKernel)
    | Transformation.Edges -> ApplyTransform.onCPU (EditType.Transformation FilterKernel.EdgesKernel)
    | Transformation.HighPass -> ApplyTransform.onCPU (EditType.Transformation FilterKernel.HighPassKernel)
    | Transformation.Laplacian -> ApplyTransform.onCPU (EditType.Transformation FilterKernel.LaplacianKernel)
    | Transformation.SobelV -> ApplyTransform.onCPU (EditType.Transformation FilterKernel.SobelVerticalKernel)
    | Transformation.Rotate -> ApplyTransform.onCPU (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> ApplyTransform.onCPU (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> ApplyTransform.onCPU (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> ApplyTransform.onCPU (EditType.Reflection Vertical)

let getTsfGPU (clContext: ClContext) localWorkSize transformation =
    match transformation with
    | Transformation.Blur ->
        ApplyTransform.onGPU clContext localWorkSize (EditType.Transformation FilterKernel.GaussianBlurKernel)
    | Transformation.Edges ->
        ApplyTransform.onGPU clContext localWorkSize (EditType.Transformation FilterKernel.EdgesKernel)
    | Transformation.HighPass ->
        ApplyTransform.onGPU clContext localWorkSize (EditType.Transformation FilterKernel.HighPassKernel)
    | Transformation.Laplacian ->
        ApplyTransform.onGPU clContext localWorkSize (EditType.Transformation FilterKernel.LaplacianKernel)
    | Transformation.SobelV ->
        ApplyTransform.onGPU clContext localWorkSize (EditType.Transformation FilterKernel.SobelVerticalKernel)
    | Transformation.Rotate -> ApplyTransform.onGPU clContext localWorkSize (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> ApplyTransform.onGPU clContext localWorkSize (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> ApplyTransform.onGPU clContext localWorkSize (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> ApplyTransform.onGPU clContext localWorkSize (EditType.Reflection Vertical)

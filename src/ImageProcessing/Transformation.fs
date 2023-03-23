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

let getTsfCPU threadsCount transformation =
    match transformation with
    | Transformation.Blur ->
        ApplyTransform(threadsCount)
            .OnCPU(EditType.Transformation FilterKernel.GaussianBlurKernel)
    | Transformation.Edges ->
        ApplyTransform(threadsCount)
            .OnCPU(EditType.Transformation FilterKernel.EdgesKernel)
    | Transformation.HighPass ->
        ApplyTransform(threadsCount)
            .OnCPU(EditType.Transformation FilterKernel.HighPassKernel)
    | Transformation.Laplacian ->
        ApplyTransform(threadsCount)
            .OnCPU(EditType.Transformation FilterKernel.LaplacianKernel)
    | Transformation.SobelV ->
        ApplyTransform(threadsCount)
            .OnCPU(EditType.Transformation FilterKernel.SobelVerticalKernel)
    | Transformation.Rotate -> ApplyTransform(threadsCount).OnCPU(EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> ApplyTransform(threadsCount).OnCPU(EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> ApplyTransform(threadsCount).OnCPU(EditType.Reflection Horizontal)
    | Transformation.ReflectV -> ApplyTransform(threadsCount).OnCPU(EditType.Reflection Vertical)

let getTsfGPU (clContext: ClContext) localWorkSize transformation =
    match transformation with
    | Transformation.Blur ->
        ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation FilterKernel.GaussianBlurKernel)
    | Transformation.Edges ->
        ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation FilterKernel.EdgesKernel)
    | Transformation.HighPass ->
        ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation FilterKernel.HighPassKernel)
    | Transformation.Laplacian ->
        ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation FilterKernel.LaplacianKernel)
    | Transformation.SobelV ->
        ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation FilterKernel.SobelVerticalKernel)
    | Transformation.Rotate -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Reflection Vertical)

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
    | Transformation.Blur -> ApplyTransform(threadsCount).OnCPU(EditType.Transformation gaussianBlurKernel)
    | Transformation.Edges -> ApplyTransform(threadsCount).OnCPU(EditType.Transformation edgesKernel)
    | Transformation.HighPass -> ApplyTransform(threadsCount).OnCPU(EditType.Transformation highPassKernel)
    | Transformation.Laplacian -> ApplyTransform(threadsCount).OnCPU(EditType.Transformation laplacianKernel)
    | Transformation.SobelV -> ApplyTransform(threadsCount).OnCPU(EditType.Transformation sobelVerticalKernel)
    | Transformation.Rotate -> ApplyTransform(threadsCount).OnCPU(EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> ApplyTransform(threadsCount).OnCPU(EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> ApplyTransform(threadsCount).OnCPU(EditType.Reflection Horizontal)
    | Transformation.ReflectV -> ApplyTransform(threadsCount).OnCPU(EditType.Reflection Vertical)

let getTsfGPU (clContext: ClContext) localWorkSize transformation =
    match transformation with
    | Transformation.Blur -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation gaussianBlurKernel)
    | Transformation.Edges -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation edgesKernel)
    | Transformation.HighPass -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation highPassKernel)
    | Transformation.Laplacian ->
        ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation laplacianKernel)
    | Transformation.SobelV ->
        ApplyTransform().OnGPU clContext localWorkSize (EditType.Transformation sobelVerticalKernel)
    | Transformation.Rotate -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> ApplyTransform().OnGPU clContext localWorkSize (EditType.Reflection Vertical)

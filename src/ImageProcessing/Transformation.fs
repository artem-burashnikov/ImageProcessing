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
    | Transformation.Blur -> applyTransformCPU (EditType.Transformation FilterKernel.GaussianBlurKernel)
    | Transformation.Edges -> applyTransformCPU (EditType.Transformation FilterKernel.EdgesKernel)
    | Transformation.HighPass -> applyTransformCPU (EditType.Transformation FilterKernel.HighPassKernel)
    | Transformation.Laplacian -> applyTransformCPU (EditType.Transformation FilterKernel.LaplacianKernel)
    | Transformation.SobelV -> applyTransformCPU (EditType.Transformation FilterKernel.SobelVerticalKernel)
    | Transformation.Rotate -> applyTransformCPU (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> applyTransformCPU (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> applyTransformCPU (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> applyTransformCPU (EditType.Reflection Vertical)

let getTsfGPU (clContext: ClContext) localWorkSize transformation =
    match transformation with
    | Transformation.Blur ->
        applyTransformGPU clContext localWorkSize (EditType.Transformation FilterKernel.GaussianBlurKernel)
    | Transformation.Edges ->
        applyTransformGPU clContext localWorkSize (EditType.Transformation FilterKernel.EdgesKernel)
    | Transformation.HighPass ->
        applyTransformGPU clContext localWorkSize (EditType.Transformation FilterKernel.HighPassKernel)
    | Transformation.Laplacian ->
        applyTransformGPU clContext localWorkSize (EditType.Transformation FilterKernel.LaplacianKernel)
    | Transformation.SobelV ->
        applyTransformGPU clContext localWorkSize (EditType.Transformation FilterKernel.SobelVerticalKernel)
    | Transformation.Rotate -> applyTransformGPU clContext localWorkSize (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> applyTransformGPU clContext localWorkSize (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> applyTransformGPU clContext localWorkSize (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> applyTransformGPU clContext localWorkSize (EditType.Reflection Vertical)

module ImageProcessing.Transformation

open ImageProcessing.ImageProcessing
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
    | Transformation.Blur -> applyTransformCPU (EditType.Transformation gaussianBlurKernel)
    | Transformation.Edges -> applyTransformCPU (EditType.Transformation edgesKernel)
    | Transformation.HighPass -> applyTransformCPU (EditType.Transformation highPassKernel)
    | Transformation.Laplacian -> applyTransformCPU (EditType.Transformation laplacianKernel)
    | Transformation.SobelV -> applyTransformCPU (EditType.Transformation sobelVerticalKernel)
    | Transformation.Rotate -> applyTransformCPU (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> applyTransformCPU (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> applyTransformCPU (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> applyTransformCPU (EditType.Reflection Vertical)

let getTsfGPU (clContext: ClContext) localWorkSize transformation =
    match transformation with
    | Transformation.Blur -> applyTransformGPU clContext localWorkSize (EditType.Transformation gaussianBlurKernel)
    | Transformation.Edges -> applyTransformGPU clContext localWorkSize (EditType.Transformation edgesKernel)
    | Transformation.HighPass -> applyTransformGPU clContext localWorkSize (EditType.Transformation highPassKernel)
    | Transformation.Laplacian -> applyTransformGPU clContext localWorkSize (EditType.Transformation laplacianKernel)
    | Transformation.SobelV -> applyTransformGPU clContext localWorkSize (EditType.Transformation sobelVerticalKernel)
    | Transformation.Rotate -> applyTransformGPU clContext localWorkSize (EditType.Rotation Clockwise)
    | Transformation.RotateCCW -> applyTransformGPU clContext localWorkSize (EditType.Rotation Counterclockwise)
    | Transformation.ReflectH -> applyTransformGPU clContext localWorkSize (EditType.Reflection Horizontal)
    | Transformation.ReflectV -> applyTransformGPU clContext localWorkSize (EditType.Reflection Vertical)

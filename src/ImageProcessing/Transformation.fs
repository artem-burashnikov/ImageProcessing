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
    | Transformation.Blur -> applyFilterCPU gaussianBlurKernel
    | Transformation.Edges -> applyFilterCPU edgesKernel
    | Transformation.HighPass -> applyFilterCPU highPassKernel
    | Transformation.Laplacian -> applyFilterCPU laplacianKernel
    | Transformation.SobelV -> applyFilterCPU sobelVerticalKernel
    | Transformation.Rotate -> rotateCPU Clockwise
    | Transformation.RotateCCW -> rotateCPU Counterclockwise
    | Transformation.ReflectH -> reflectCPU Horizontal
    | Transformation.ReflectV -> reflectCPU Vertical

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

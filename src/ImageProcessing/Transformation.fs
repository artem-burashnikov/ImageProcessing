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

let getTsfGPU (clContext: ClContext) localWorkSize transformation : (Image -> Image) =

    let filterKernel = lazy Kernel<_>.makeFilterKernel clContext localWorkSize
    let rotationKernel = lazy Kernel<_>.makeRotationKernel clContext localWorkSize
    let reflectionKernel = lazy Kernel<_>.makeReflectionKernel clContext localWorkSize

    match transformation with
    | Transformation.Blur -> applyFilterGPU (filterKernel.Force()) clContext gaussianBlurKernel
    | Transformation.Edges -> applyFilterGPU (filterKernel.Force()) clContext edgesKernel
    | Transformation.HighPass -> applyFilterGPU (filterKernel.Force()) clContext highPassKernel
    | Transformation.Laplacian -> applyFilterGPU (filterKernel.Force()) clContext laplacianKernel
    | Transformation.SobelV -> applyFilterGPU (filterKernel.Force()) clContext sobelVerticalKernel
    | Transformation.Rotate -> rawProcessGPU (rotationKernel.Force () Clockwise) clContext
    | Transformation.RotateCCW -> rawProcessGPU (rotationKernel.Force () Counterclockwise) clContext
    | Transformation.ReflectH -> rawProcessGPU (reflectionKernel.Force () Horizontal) clContext
    | Transformation.ReflectV -> rawProcessGPU (reflectionKernel.Force () Vertical) clContext

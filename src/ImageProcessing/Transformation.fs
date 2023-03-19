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

    let filterKernel = Kernel<_>.makeFilterKernel clContext localWorkSize

    match transformation with
    | Transformation.Blur -> applyFilterGPU filterKernel clContext gaussianBlurKernel
    | Transformation.Edges -> applyFilterGPU filterKernel clContext edgesKernel
    | Transformation.HighPass -> applyFilterGPU filterKernel clContext highPassKernel
    | Transformation.Laplacian -> applyFilterGPU filterKernel clContext laplacianKernel
    | Transformation.SobelV -> applyFilterGPU filterKernel clContext sobelVerticalKernel

    | Transformation.Rotate ->
        let rotationKernel = Kernel<_>.makeRotationKernel clContext localWorkSize Clockwise

        rawProcessGPU rotationKernel clContext

    | Transformation.RotateCCW ->
        let rotationKernel =
            Kernel<_>.makeRotationKernel clContext localWorkSize Counterclockwise

        rawProcessGPU rotationKernel clContext

    | Transformation.ReflectH ->
        let reflectionKernel =
            Kernel<_>.makeReflectionKernel clContext localWorkSize Horizontal

        rawProcessGPU reflectionKernel clContext

    | Transformation.ReflectV ->
        let reflectionKernel =
            Kernel<_>.makeReflectionKernel clContext localWorkSize Vertical

        rawProcessGPU reflectionKernel clContext

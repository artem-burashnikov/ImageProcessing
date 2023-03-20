module ImageProcessing.Transformation

open ImageProcessing.ImageProcessing

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

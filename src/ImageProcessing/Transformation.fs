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

let getTsf applicatorFunction (transformation: Transformation) =
    match transformation with
    | Transformation.Blur -> applicatorFunction gaussianBlurKernel
    | Transformation.Edges -> applicatorFunction edgesKernel
    | Transformation.HighPass -> applicatorFunction highPassKernel
    | Transformation.Laplacian -> applicatorFunction laplacianKernel
    | Transformation.SobelV -> applicatorFunction sobelVerticalKernel
    | Transformation.Rotate -> rotate90Clockwise
    | Transformation.RotateCCW -> rotate90Counterclockwise

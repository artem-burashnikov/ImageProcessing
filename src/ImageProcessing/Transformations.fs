module ImageProcessing.Transformations

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

let getTransformation (transformation: Transformation) =
    match transformation with
    | Transformation.Blur -> applyFilter gaussianBlurKernel
    | Transformation.Edges -> applyFilter edgesKernel
    | Transformation.HighPass -> applyFilter highPassKernel
    | Transformation.Laplacian -> applyFilter laplacianKernel
    | Transformation.SobelV -> applyFilter sobelVerticalKernel
    | Transformation.Rotate -> rotate90Clockwise
    | Transformation.RotateCCW -> rotate90Counterclockwise

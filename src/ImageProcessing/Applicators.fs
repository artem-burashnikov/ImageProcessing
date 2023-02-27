module Applicators

open ImageProcessing.ImageProcessing

[<RequireQualifiedAccess>]
type Applicator =
    | Blur
    | Edges
    | HighPass
    | Laplacian
    | SobelV
    | Rotate
    | RotateCCW

    static member ApplicatorFromStr str =
        match str with
        | "blur" -> Blur
        | "edges" -> Edges
        | "highpass" -> HighPass
        | "laplacian" -> Laplacian
        | "sobelv" -> SobelV
        | "rotate" -> Rotate
        | "rotateccw" -> RotateCCW
        | s -> failwith $"{s} applicator is undefined."

/// Outputs a function to be used on a 2d array.
let getApplicator (filter: Applicator) =
    match filter with
    | Applicator.Blur -> applyFilter gaussianBlurKernel
    | Applicator.Edges -> applyFilter edgesKernel
    | Applicator.HighPass -> applyFilter highPassKernel
    | Applicator.Laplacian -> applyFilter laplacianKernel
    | Applicator.SobelV -> applyFilter sobelVerticalKernel
    | Applicator.Rotate -> rotate90Clockwise
    | Applicator.RotateCCW -> rotate90Counterclockwise

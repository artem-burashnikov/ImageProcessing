module FilterApplicators

open ImageProcessing.ImageProcessing

[<RequireQualifiedAccess>]
type Filter =
    | Blur
    | Edges
    | HighPass
    | Laplacian
    | Sobel
    | Rotate
    | RotateCWW
    | Invalid

    static member FilterFromStr str =
        match str with
        | "blur" -> Blur
        | "edges" -> Edges
        | "highpass" -> HighPass
        | "laplacian" -> Laplacian
        | "sobel" -> Sobel
        | "rotate" -> Rotate
        | "rotatecww" -> RotateCWW
        | _ -> Invalid

/// Following properties provide a function to be used on a 2d array.
let getApplicator (filter: Filter) =
    match filter with
    | Filter.Blur -> applyFilter gaussianBlurKernel
    | Filter.Edges -> applyFilter edgesKernel
    | Filter.HighPass -> applyFilter highPassKernel
    | Filter.Laplacian -> applyFilter laplacianKernel
    | Filter.Sobel -> applyFilter sobelKernel
    | Filter.Rotate -> rotate90Clockwise
    | Filter.RotateCWW -> rotate90Counterclockwise
    | Filter.Invalid -> failwith "Not yet implemented"

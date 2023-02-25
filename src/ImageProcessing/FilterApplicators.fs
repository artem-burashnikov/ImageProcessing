module FilterApplicators

open ImageProcessing.ImageProcessing

/// Following methods output a function to be used on a 2d array.
type Applicator =
    static member Blur = applyFilter gaussianBlurKernel
    static member Edges = applyFilter edgesKernel
    static member HighPass = applyFilter highPassKernel
    static member Laplacian = applyFilter laplacianKernel
    static member Sobel = applyFilter sobelKernel
    /// Clockwise rotation
    static member Rotate = rotate90Clockwise
    /// Counterclockwise rotation
    static member RotateCCW = rotate90Counterclockwise

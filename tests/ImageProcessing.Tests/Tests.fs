namespace ImageProcessing.Tests

open System
open Expecto
open ImageProcessing.ImageProcessing

module TestSamples =

    let r = Random()

    /// Applies filter kernel on a 2d table
    let applyFilter2DArray (filter: float32[][]) (img: byte[,]) =
        let height = Array2D.length1 img
        let width = Array2D.length2 img

        let filterD = (Array.length filter) / 2

        let filter = Array.concat filter

        let processPixel px py =
            let dataToHandle =
                [| for i in px - filterD .. px + filterD do
                       for j in py - filterD .. py + filterD do
                           if i < 0 || i >= height || j < 0 || j >= width then
                               float32 img[px, py]
                           else
                               float32 img[i, j] |]

            Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

        Array2D.mapi (fun x y _ -> byte (processPixel x y)) img

    let flatArray2D array2D =
        [| for x in 0 .. (Array2D.length1 array2D) - 1 do
               for y in 0 .. (Array2D.length2 array2D) - 1 do
                   yield array2D[x, y] |]

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testProperty "Rotating clockwise 4 times outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  // "+2" because minimum testing for 2x2 tables
                  let w, h = Convert.ToInt32 width + 2, Convert.ToInt32 height + 2
                  let data = Array.init (w * h) (fun _ -> byte (r.Next(0, 256)))
                  let originalImg = Image(data, w, h, "")

                  let rotatedImg =
                      originalImg
                      |> rotate90Clockwise
                      |> rotate90Clockwise
                      |> rotate90Clockwise
                      |> rotate90Clockwise

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Clockwise rotated 4 times failed to match the original image"

              testProperty "Rotating counterclockwise 4 times outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  // "+2" because minimum testing for 2x2 tables
                  let w, h = Convert.ToInt32 width + 2, Convert.ToInt32 height + 2
                  let data = Array.init (w * h) (fun _ -> byte (r.Next(0, 256)))
                  let originalImg = Image(data, w, h, "")

                  let rotatedImg =
                      originalImg
                      |> rotate90Counterclockwise
                      |> rotate90Counterclockwise
                      |> rotate90Counterclockwise
                      |> rotate90Counterclockwise

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Counterclockwise rotated 4 times failed to match the original image"

              testProperty "Rotating clockwise then counterclockwise outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  // "+2" because minimum testing for 2x2 tables
                  let w, h = Convert.ToInt32 width + 2, Convert.ToInt32 height + 2
                  let data = Array.init (w * h) (fun _ -> byte (r.Next(0, 256)))
                  let originalImg = Image(data, w, h, "")

                  let rotatedImg = rotate90Clockwise originalImg |> rotate90Counterclockwise

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Clockwise and then counterclockwise failed to match the original image"

              testProperty "Applying filter to a 2DArray and a 1DArray should produce the same output"
              <| fun (width: uint) (height: uint) ->
                  let w, h = Convert.ToInt32 width + 2, Convert.ToInt32 height + 2

                  let data2D = Array2D.init h w (fun _ _ -> byte (r.Next(0, 256)))

                  let data1D = flatArray2D data2D

                  let actualResult = applyFilter edgesKernel (Image(data1D, w, h, ""))

                  let expectedResult = applyFilter2DArray edgesKernel data2D |> flatArray2D

                  Expect.equal actualResult.Data expectedResult $"data1D: %A{data1D},\ndata2D:%A{data2D}" ]

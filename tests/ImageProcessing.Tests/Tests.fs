namespace ImageProcessing.Tests

open Expecto
open ImageProcessing.ImageProcessing

module TestSamples =

    let src = __SOURCE_DIRECTORY__
    let testSamplesFolder = "TestSamples"
    let imagesFolder = "Images"
    let inputFolder = "Input"
    let outputFolder = "Output"
    let inputImg = "sample.jpg"
    let outputImg = "out_sample.jpg"
    let fp = System.IO.Path.Combine(testSamplesFolder, imagesFolder)

    let r = System.Random()

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testProperty "Rotating clockwise 4 times outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  // "+2" because minimum testing for 2x2 tables
                  let w, h = System.Convert.ToInt32 width + 2, System.Convert.ToInt32 height + 2
                  let originalImg = Array2D.init w h (fun _ _ -> byte (r.Next(0, 256)))

                  let rotatedImg =
                      rotate90Clockwise originalImg
                      |> rotate90Clockwise
                      |> rotate90Clockwise
                      |> rotate90Clockwise

                  Expect.equal rotatedImg originalImg "Clockwise rotated 4 times failed to match the original image"

              testProperty "Rotating counterclockwise 4 times outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  // "+2" because minimum testing for 2x2 tables
                  let w, h = System.Convert.ToInt32 width + 2, System.Convert.ToInt32 height + 2
                  let originalImg = Array2D.init w h (fun _ _ -> byte (r.Next(0, 256)))

                  let rotatedImg =
                      rotate90Counterclockwise originalImg
                      |> rotate90Counterclockwise
                      |> rotate90Counterclockwise
                      |> rotate90Counterclockwise

                  Expect.equal
                      rotatedImg
                      originalImg
                      "Counterclockwise rotated 4 times failed to match the original image"

              testProperty "Rotating clockwise then counterclockwise outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  // "+2" because minimum testing for 2x2 tables
                  let w, h = System.Convert.ToInt32 width + 2, System.Convert.ToInt32 height + 2
                  let originalImg = Array2D.init w h (fun _ _ -> byte (r.Next(0, 256)))

                  let rotatedImg = rotate90Clockwise originalImg |> rotate90Counterclockwise

                  Expect.equal
                      rotatedImg
                      originalImg
                      "Clockwise and then counterclockwise failed to match the original image" ]

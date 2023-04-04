namespace ImageProcessing.Tests

open System
open Expecto
open FsCheck
open ImageProcessing.ImageProcessing

module TestHelperFunctions =
    let r = Random()

    /// Applies filter kernel to a 2D-table
    let applyFilter2DArray (filter: float32[,]) (img: byte[,]) =
        let height = Array2D.length1 img
        let width = Array2D.length2 img

        let filterD = (Array2D.length1 filter) / 2

        let filter = flattenArray2D filter

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

    // Applies filter kernel to a 1D-array
    let applyFilterNaive (filter: float32[,]) (img: Image) =
        let height = img.Height
        let width = img.Width

        let filterD = (Array2D.length1 filter) / 2

        let filter = flattenArray2D filter

        let processPixel p =
            let pi = p / width
            let pj = p % width

            let dataToHandle =
                [| for i in pi - filterD .. pi + filterD do
                       for j in pj - filterD .. pj + filterD do
                           if i < 0 || i >= height || j < 0 || j >= width then
                               float32 img.Data[p]
                           else
                               float32 img.Data[i * width + j] |]

            Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

        let data = Array.mapi (fun i _ -> byte (processPixel i)) img.Data
        Image(data, width, height, img.Name)

    let getImage arr2d =
        Image(flattenArray2D arr2d, Array2D.length2 arr2d, Array2D.length1 arr2d, "sample.jpg")

module Generators =

    open TestHelperFunctions

    type ImageData = ImageData of byte[,]

    let ImageArb () =

        let rows = r.Next(2, 500)
        let columns = r.Next(2, 500)

        Gen.array2DOfDim (rows, columns) Arb.generate
        |> Arb.fromGen
        |> Arb.convert ImageData (fun (ImageData l) -> l)

    let addToConfig config =
        { config with arbitrary = typeof<ImageData>.DeclaringType :: config.arbitrary }

[<AutoOpen>]
module Auto =
    let private config = Generators.addToConfig FsCheckConfig.defaultConfig
    let testCustomProp name = testPropertyWithConfig config name

module CPUTests =

    open TestHelperFunctions

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCustomProp "Rotating clockwise 4 times outputs the original image"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let rotatedImg =
                      originalImg
                      |> rotateCPU Clockwise
                      |> rotateCPU Clockwise
                      |> rotateCPU Clockwise
                      |> rotateCPU Clockwise

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Clockwise rotated 4 times failed to match the original image"

              testCustomProp "Rotating counterclockwise 4 times outputs the original image"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let rotatedImg =
                      originalImg
                      |> rotateCPU Counterclockwise
                      |> rotateCPU Counterclockwise
                      |> rotateCPU Counterclockwise
                      |> rotateCPU Counterclockwise

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Counterclockwise rotated 4 times failed to match the original image"

              testCustomProp "Rotating clockwise then counterclockwise outputs the original image"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let rotatedImg = originalImg |> rotateCPU Clockwise |> rotateCPU Counterclockwise

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Clockwise and then counterclockwise failed to match the original image"

              testCustomProp "Applying filter to a 2DArray and a 1DArray should produce the same output"
              <| fun (Generators.ImageData arr2d) ->

                  let height = Array2D.length1 arr2d
                  let width = Array2D.length2 arr2d

                  let data1D = flattenArray2D arr2d

                  let actualResult = applyFilterNaive edgesKernel (Image(data1D, width, height, ""))

                  let expectedResult = applyFilter2DArray edgesKernel arr2d |> flattenArray2D

                  Expect.equal actualResult.Data expectedResult $"data1D: %A{data1D},\ndata2D:%A{arr2d}"

              testCustomProp "Consecutively reflecting horizontally two times outputs the original pixel data"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let reflectedImage = originalImg |> reflectCPU Horizontal |> reflectCPU Horizontal

                  Expect.equal
                      reflectedImage.Data
                      originalImg.Data
                      "Reflecting horizontally two times failed to match the original"

              testCustomProp "Consecutively reflecting vertically two times outputs the original pixel data"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let reflectedImage = originalImg |> reflectCPU Vertical |> reflectCPU Vertical

                  Expect.equal
                      reflectedImage.Data
                      originalImg.Data
                      "Reflecting vertically two times failed to match the original"

              testCustomProp "Horizontal reflection is the same as vertical reflection followed by two rotations"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult = originalImg |> reflectCPU Horizontal

                  let expectedResult =
                      originalImg |> reflectCPU Vertical |> rotateCPU Clockwise |> rotateCPU Clockwise

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Horizontal reflection failed to match vertical reflection followed by two rotations"

              testCustomProp "Vertical reflection is the same as horizontal reflection followed by two rotations"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult = originalImg |> reflectCPU Vertical

                  let expectedResult =
                      originalImg
                      |> reflectCPU Horizontal
                      |> rotateCPU Clockwise
                      |> rotateCPU Clockwise

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Vertical reflection failed to match horizontal reflection followed by two rotations" ]

module GeneralTests =

    let r = Random()

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "VirtualArray.splitInto given a 0 count"
              <| fun _ ->
                  let arr = VirtualArray([| 0 |], 0, 1)

                  let actualResult =
                      Expect.throws (fun _ -> VirtualArray.splitInto 0 arr |> ignore) "Count <= 0 is not defined"

                  actualResult

              testProperty "VirtualArray splitInto should match Array splitInto"
              <| fun (memory: array<_>) ->

                  // Counts <= 0 will throw an exception in custom and built-in methods.
                  let count = r.Next(1, memory.Length * 2 + 1)

                  // Get a random starting index from a given memory to initialize a VirtualArray
                  let head = r.Next(0, memory.Length)
                  let vArray = VirtualArray(memory, head, memory.Length - head)

                  // Use custom method to split a VirtualArray ...
                  let actualResult =
                      let arr = VirtualArray.splitInto count vArray
                      // ... and convert split VirtualArrays into actual arrays
                      let res = Array.zeroCreate arr.Length

                      for i in 0 .. arr.Length - 1 do
                          res[i] <- arr[i].Memory[arr[i].Head .. arr[i].Head + arr[i].Length - 1]

                      res

                  // Apply build-int splitInto method
                  let expectedResult = Array.splitInto count memory[head..]

                  // Results should match
                  Expect.equal actualResult expectedResult ""

              testProperty "VirtualArray.mapi equals Array.mapi"
              <| fun (memory: array<byte>) ->
                  // Make a copy of original data so changes differ.
                  let actualResult = Array.copy memory

                  // Virtually split the copy in two parts ...
                  let head = r.Next(0, memory.Length)
                  let part1 = VirtualArray(actualResult, 0, head)
                  let part2 = VirtualArray(actualResult, head, actualResult.Length - head)

                  // ... and apply mapi on virtual parts separately.
                  VirtualArray.mapi (fun i value -> byte i + value) part1 |> ignore
                  VirtualArray.mapi (fun i value -> byte i + value) part2 |> ignore

                  // Apply mapi on original data
                  let expectedResult = Array.mapi (fun i value -> byte i + value) memory

                  // Results should be the same
                  Expect.equal actualResult[head..] expectedResult[head..] "" ]

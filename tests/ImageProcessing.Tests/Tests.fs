namespace ImageProcessing.Tests

open System
open Expecto
open ImageProcessing.ImageProcessing

module ImageTransformationTests =

    let r = Random()

    /// Applies filter kernel to a 2D-table
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

    // Applies filter kernel to a 1D-array
    let applyFilterNaive (filter: float32[][]) (img: Image) =
        let height = img.Height
        let width = img.Width

        let filterD = (Array.length filter) / 2

        let filter = Array.concat filter

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

    // Initialize 1D-array from given width and height
    let initDataFromWH (width: uint) (height: uint) =
        let w, h = Convert.ToInt32 width + 2, Convert.ToInt32 height + 2
        let data = Array.init (w * h) (fun _ -> byte (r.Next(0, 256)))
        data, w, h

    // 2D-table to 1D-array
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
                  let data, w, h = initDataFromWH width height
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
                  let data, w, h = initDataFromWH width height
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
                  let data, w, h = initDataFromWH width height
                  let originalImg = Image(data, w, h, "")

                  let rotatedImg = originalImg |> rotate90Clockwise |> rotate90Counterclockwise

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Clockwise and then counterclockwise failed to match the original image"

              testProperty "Applying filter to a 2DArray and a 1DArray should produce the same output"
              <| fun (width: uint) (height: uint) ->
                  let w, h = Convert.ToInt32 width + 2, Convert.ToInt32 height + 2

                  let data2D = Array2D.init h w (fun _ _ -> byte (r.Next(0, 256)))

                  let data1D = flatArray2D data2D

                  let actualResult = applyFilterNaive edgesKernel (Image(data1D, w, h, ""))

                  let expectedResult = applyFilter2DArray edgesKernel data2D |> flatArray2D

                  Expect.equal actualResult.Data expectedResult $"data1D: %A{data1D},\ndata2D:%A{data2D}" ]

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

namespace ImageProcessing.Tests

open System
open Expecto
open ImageProcessing.ImageProcessing
open ImageProcessing.Transformation
open ImageProcessing.FilterKernel
open Brahma.FSharp

module TestHelperFunctions =
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

    let getImage width height =
        let data, w, h = initDataFromWH width height
        Image(data, w, h, "")

module CPUTests =

    open TestHelperFunctions

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testProperty "Rotating clockwise 4 times outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let rotatedImg =
                      originalImg
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Clockwise rotated 4 times failed to match the original image"

              testProperty "Rotating counterclockwise 4 times outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let rotatedImg =
                      originalImg
                      |> ApplyTransform().OnCPU(EditType.Rotation Counterclockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Counterclockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Counterclockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Counterclockwise)

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Counterclockwise rotated 4 times failed to match the original image"

              testProperty "Rotating clockwise then counterclockwise outputs the original image"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let rotatedImg =
                      originalImg
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Counterclockwise)

                  Expect.equal
                      rotatedImg.Data
                      originalImg.Data
                      "Clockwise and then counterclockwise failed to match the original image"

              testProperty "Applying filter to a 2DArray and a 1DArray should produce the same output"
              <| fun (width: uint) (height: uint) ->
                  let w, h = Convert.ToInt32 width + 2, Convert.ToInt32 height + 2

                  let data2D = Array2D.init h w (fun _ _ -> byte (r.Next(0, 256)))

                  let data1D = flatArray2D data2D

                  let actualResult =
                      applyFilterNaive FilterKernel.EdgesKernel (Image(data1D, w, h, ""))

                  let expectedResult =
                      applyFilter2DArray FilterKernel.EdgesKernel data2D |> flatArray2D

                  Expect.equal actualResult.Data expectedResult $"data1D: %A{data1D},\ndata2D:%A{data2D}"

              testProperty "Consecutively reflecting horizontally two times outputs the original pixel data"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let reflectedImage =
                      originalImg
                      |> ApplyTransform().OnCPU(EditType.Reflection Horizontal)
                      |> ApplyTransform().OnCPU(EditType.Reflection Horizontal)

                  Expect.equal
                      reflectedImage.Data
                      originalImg.Data
                      "Reflecting horizontally two times failed to match the original"

              testProperty "Consecutively reflecting vertically two times outputs the original pixel data"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let reflectedImage =
                      originalImg
                      |> ApplyTransform().OnCPU(EditType.Reflection Vertical)
                      |> ApplyTransform().OnCPU(EditType.Reflection Vertical)

                  Expect.equal
                      reflectedImage.Data
                      originalImg.Data
                      "Reflecting vertically two times failed to match the original"

              testProperty "Horizontal reflection is the same as vertical reflection followed by two rotations"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let actualResult =
                      originalImg |> ApplyTransform().OnCPU(EditType.Reflection Horizontal)

                  let expectedResult =
                      originalImg
                      |> ApplyTransform().OnCPU(EditType.Reflection Vertical)
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Horizontal reflection failed to match vertical reflection followed by two rotations"

              testProperty "Vertical reflection is the same as horizontal reflection followed by two rotations"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let actualResult =
                      originalImg |> ApplyTransform().OnCPU(EditType.Reflection Vertical)

                  let expectedResult =
                      originalImg
                      |> ApplyTransform().OnCPU(EditType.Reflection Horizontal)
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)
                      |> ApplyTransform().OnCPU(EditType.Rotation Clockwise)

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
                  Expect.equal actualResult expectedResult "" ]

module GPUTests =

    open TestHelperFunctions

    // Initialize parameters for GPU
    let device = ClDevice.GetFirstAppropriateDevice()
    let context = ClContext(device)

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testProperty
                  "Applying clockwise rotation on CPU and applying rotation on GPU has to yield the same pixel data"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let gpuApplicator = getTsfGPU context 64 Transformation.Rotate

                  let cpuApplicator = getTsfCPU 1 Transformation.Rotate

                  let actualResult = originalImg |> gpuApplicator

                  let expectedResult = originalImg |> cpuApplicator

                  Expect.equal actualResult.Data expectedResult.Data "Clockwise rotations on GPU and CPU don't match"

              testProperty
                  "Applying counterclockwise rotation on CPU and applying rotation on GPU has to yield the same pixel data"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let gpuApplicator = getTsfGPU context 64 Transformation.RotateCCW

                  let cpuApplicator = getTsfCPU 1 Transformation.RotateCCW

                  let actualResult = originalImg |> gpuApplicator

                  let expectedResult = originalImg |> cpuApplicator

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Counterclockwise rotations on GPU and CPU don't match"

              testProperty "Applying available filters on CPU and on GPU has to yield the same pixel data"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let filters =
                      [ Transformation.Blur
                        Transformation.Edges
                        Transformation.HighPass
                        Transformation.Laplacian
                        Transformation.SobelV ]

                  let gpuApplicators = List.map (getTsfGPU context 64) filters

                  let cpuApplicators = List.map (getTsfCPU 1) filters

                  let actualResult =
                      List.fold (fun img transformation -> transformation img) originalImg gpuApplicators

                  let expectedResult =
                      List.fold (fun img transformation -> transformation img) originalImg cpuApplicators

                  Expect.equal actualResult.Data expectedResult.Data "Application of filters on GPU and CPU don't match"

              testProperty "Applying horizontal reflection on CPU and on GPU has to yield the same pixel data"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let gpuApplicator = getTsfGPU context 64 Transformation.ReflectH

                  let cpuApplicator = getTsfCPU 1 Transformation.ReflectH

                  let actualResult = originalImg |> gpuApplicator

                  let expectedResult = originalImg |> cpuApplicator

                  Expect.equal actualResult.Data expectedResult.Data "Horizontal reflection on GPU and CPU don't match"

              testProperty "Applying vertical reflection on CPU and on GPU has to yield the same pixel data"
              <| fun (width: uint) (height: uint) ->
                  let originalImg = getImage width height

                  let gpuApplicator = getTsfGPU context 64 Transformation.ReflectV

                  let cpuApplicator = getTsfCPU 1 Transformation.ReflectV

                  let actualResult = originalImg |> gpuApplicator

                  let expectedResult = originalImg |> cpuApplicator

                  Expect.equal actualResult.Data expectedResult.Data "Vertical reflection on GPU and CPU don't match" ]

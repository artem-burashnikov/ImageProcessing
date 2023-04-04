namespace ImageProcessing.Tests

open System
open Expecto
open FsCheck
open ImageProcessing.ImageProcessing
open ImageProcessing.FilterKernel
open Brahma.FSharp

module TestHelperFunctions =

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

    let transform = ApplyTransform()

module Generators =

    type ImageData = ImageData of byte[,]

    let ImageDataArb () =

        let values = Gen.choose (2, 100) |> Gen.sample 0 2

        let rows = values[0]
        let columns = values[1]

        Gen.array2DOfDim (rows, columns) Arb.generate
        |> Arb.fromGen
        |> Arb.convert ImageData (fun (ImageData l) -> l)

    type FilterKernel = FilterKernel of float32[,]

    let filterKernelArb () =

        let value = Gen.choose (0, 5) |> Gen.sample 0 1 |> List.head

        let size = 2 * value + 1

        Gen.array2DOfDim (size, size) Arb.generate
        |> Arb.fromGen
        |> Arb.convert FilterKernel (fun (FilterKernel l) -> l)

    let addToConfig config =
        { config with
            arbitrary =
                typeof<FilterKernel>.DeclaringType
                :: (typeof<ImageData>.DeclaringType :: config.arbitrary) }

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

                  let expectedResult = getImage arr2d

                  let actualResult =
                      expectedResult
                      |> transform.OnCPU(EditType.Rotation Clockwise)
                      |> transform.OnCPU(EditType.Rotation Clockwise)
                      |> transform.OnCPU(EditType.Rotation Clockwise)
                      |> transform.OnCPU(EditType.Rotation Clockwise)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Clockwise rotated 4 times failed to match the original image"

              testCustomProp "Rotating counterclockwise 4 times outputs the original image"
              <| fun (Generators.ImageData arr2d) ->

                  let expectedResult = getImage arr2d

                  let actualResult =
                      expectedResult
                      |> transform.OnCPU(EditType.Rotation Counterclockwise)
                      |> transform.OnCPU(EditType.Rotation Counterclockwise)
                      |> transform.OnCPU(EditType.Rotation Counterclockwise)
                      |> transform.OnCPU(EditType.Rotation Counterclockwise)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Counterclockwise rotated 4 times failed to match the original image"

              testCustomProp "Rotating clockwise then counterclockwise outputs the original image"
              <| fun (Generators.ImageData arr2d) ->

                  let expectedResult = getImage arr2d

                  let actualResult =
                      expectedResult
                      |> transform.OnCPU(EditType.Rotation Clockwise)
                      |> transform.OnCPU(EditType.Rotation Counterclockwise)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
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

                  let expectedResult = getImage arr2d

                  let actualResult =
                      expectedResult
                      |> transform.OnCPU(EditType.Reflection Horizontal)
                      |> transform.OnCPU(EditType.Reflection Horizontal)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Reflecting horizontally two times failed to match the original"

              testCustomProp "Consecutively reflecting vertically two times outputs the original pixel data"
              <| fun (Generators.ImageData arr2d) ->

                  let expectedResult = getImage arr2d

                  let actualResult =
                      expectedResult
                      |> transform.OnCPU(EditType.Reflection Vertical)
                      |> transform.OnCPU(EditType.Reflection Vertical)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Reflecting vertically two times failed to match the original"

              testCustomProp "Horizontal reflection is the same as vertical reflection followed by two rotations"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult = originalImg |> transform.OnCPU(EditType.Reflection Horizontal)

                  let expectedResult =
                      originalImg
                      |> transform.OnCPU(EditType.Reflection Vertical)
                      |> transform.OnCPU(EditType.Rotation Clockwise)
                      |> transform.OnCPU(EditType.Rotation Clockwise)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Horizontal reflection failed to match vertical reflection followed by two rotations"

              testCustomProp "Vertical reflection is the same as horizontal reflection followed by two rotations"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult = originalImg |> transform.OnCPU(EditType.Reflection Vertical)

                  let expectedResult =
                      originalImg
                      |> transform.OnCPU(EditType.Reflection Horizontal)
                      |> transform.OnCPU(EditType.Rotation Clockwise)
                      |> transform.OnCPU(EditType.Rotation Clockwise)

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Vertical reflection failed to match horizontal reflection followed by two rotations" ]

module GeneralTests =

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
              <| fun (Generators.ImageData arr2d) ->

                  let memory = flattenArray2D arr2d

                  // Counts <= 0 will throw an exception in custom and built-in methods.
                  let count = Gen.choose (1, memory.Length * 2 + 1) |> Gen.sample 0 1 |> List.head // r.Next(1, memory.Length * 2 + 1)

                  // Get a random starting index from a given memory to initialize a VirtualArray
                  let head = Gen.choose (0, memory.Length - 1) |> Gen.sample 0 1 |> List.head // r.Next(0, memory.Length)

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
            [ testCustomProp
                  "Applying clockwise rotation on CPU and applying rotation on GPU has to yield the same pixel data"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult =
                      transform.OnGPU context 64 (EditType.Rotation Clockwise) originalImg

                  let expectedResult = transform.OnCPU (EditType.Rotation Clockwise) originalImg

                  Expect.equal actualResult.Data expectedResult.Data "Clockwise rotations on GPU and CPU don't match"

              testCustomProp
                  "Applying counterclockwise rotation on CPU and applying rotation on GPU has to yield the same pixel data"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult =
                      transform.OnGPU context 64 (EditType.Rotation Counterclockwise) originalImg

                  let expectedResult =
                      transform.OnCPU (EditType.Rotation Counterclockwise) originalImg

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Counterclockwise rotations on GPU and CPU don't match"

              testCustomProp "Applying available filters on CPU and on GPU has to yield the same pixel data"
              <| fun (Generators.ImageData arr2d) ->
                  let transformations = EditType.all

                  let img = getImage arr2d

                  let applyTransformOnGPU =
                      (fun image (state: EditType) -> transform.OnGPU context 64 state image)

                  let applyTransformOnCPU =
                      (fun image (state: EditType) -> transform.OnCPU state image)

                  let actualResult = Array.fold applyTransformOnGPU img transformations

                  let expectedResult = Array.fold applyTransformOnCPU img transformations

                  Expect.equal actualResult.Data expectedResult.Data "Application of filters on GPU and CPU don't match"

              testCustomProp "Applying horizontal reflection on CPU and on GPU has to yield the same pixel data"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult =
                      transform.OnGPU context 64 (EditType.Reflection Horizontal) originalImg

                  let expectedResult = transform.OnCPU (EditType.Reflection Horizontal) originalImg

                  Expect.equal actualResult.Data expectedResult.Data "Horizontal reflection on GPU and CPU don't match"

              testCustomProp "Applying vertical reflection on CPU and on GPU has to yield the same pixel data"
              <| fun (Generators.ImageData arr2d) ->

                  let originalImg = getImage arr2d

                  let actualResult =
                      transform.OnGPU context 64 (EditType.Reflection Vertical) originalImg

                  let expectedResult = transform.OnCPU (EditType.Reflection Vertical) originalImg

                  Expect.equal actualResult.Data expectedResult.Data "Vertical reflection on GPU and CPU don't match" ]

module PixelMatrixProcessingTests =

    open TestHelperFunctions

    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCustomProp
                  "Filter application: Utilizing virtual split to process an image data should match the process without virtual split"
              <| fun (Generators.ImageData arr2d) (Generators.FilterKernel kernel) ->

                  let height = Array2D.length1 arr2d
                  let width = Array2D.length2 arr2d

                  let edit = EditType.Transformation kernel

                  let img = getImage arr2d

                  let numCores = min (Environment.ProcessorCount - 1) (width * height)

                  let multiThreadingTransform = ApplyTransform(numCores)

                  let actualResult = multiThreadingTransform.OnCPU edit img
                  let expectedResult = transform.OnCPU edit img

                  Expect.equal
                      actualResult.Data
                      expectedResult.Data
                      "Applying filters utilizing virtual split produced an error" ]

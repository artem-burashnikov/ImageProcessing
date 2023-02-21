namespace ImageProcessing

open Brahma.FSharp

module Main =
    let pathToExamples = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "images")
    let inputFolder = System.IO.Path.Combine(pathToExamples, "input")
    let outputFolder = System.IO.Path.Combine(pathToExamples, "output")


    [<EntryPoint>]
    let main (argv: string array) =

        (*
        let amdDevice = ClDevice.GetAvailableDevices(platform = Platform.Amd) |> Seq.head

        let amdContext = ClContext(amdDevice)
        let applyFiltersOnAmdGPU = ImageProcessing.applyFiltersGPU amdContext 64

        let filters =
            [ ImageProcessing.gaussianBlurKernel
              ImageProcessing.gaussianBlurKernel
              ImageProcessing.edgesKernel ]

        // let grayscaleImage = ImageProcessing.loadAs2DArray demoFile
        // let blur = ImageProcessing.applyFilter ImageProcessing.gaussianBlurKernel grayscaleImage
        // let edges = ImageProcessing.applyFilter ImageProcessing.edgesKernel blur
        // let edges = applyFiltersGPU [ImageProcessing.gaussianBlurKernel; ImageProcessing.edgesKernel] grayscaleImage
        // ImageProcessing.save2DByteArrayAsImage edges ""

        let start = System.DateTime.Now

        Streaming.processAllFiles inputFolder outputFolder [ applyFiltersOnAmdGPU filters ]

        printfn $"TotalTime = %f{(System.DateTime.Now - start).TotalMilliseconds}"
        *)
        0

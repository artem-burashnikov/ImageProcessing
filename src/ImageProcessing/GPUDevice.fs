module ImageProcessing.GPUDevice

open Brahma.FSharp
let localWorkSize = 64

let device = ClDevice.GetFirstAppropriateDevice()
let context = ClContext(device)

let noGPU () =
    Seq.isEmpty (ClDevice.GetAvailableDevices())

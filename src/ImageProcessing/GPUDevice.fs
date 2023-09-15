/// <summary>
/// Provides utilities for GPU device management and detection.
/// </summary>
module ImageProcessing.GPUDevice

open Brahma.FSharp

/// <summary>
/// The default local workgroup size for GPU operations.
/// </summary>
let localWorkSize = 64

/// <summary>
/// Represents the default GPU device for image processing operations.
/// </summary>
let device = ClDevice.GetFirstAppropriateDevice()
/// <summary>
/// Represents the the OpenCL context for GPU operations.
/// </summary>
let context = ClContext(device)

/// <summary>
/// Checks if there are available GPU devices for image processing.
/// </summary>
/// <returns>True if there are no available GPU devices; otherwise, false.</returns>
let noGPU () =
    Seq.isEmpty (ClDevice.GetAvailableDevices())

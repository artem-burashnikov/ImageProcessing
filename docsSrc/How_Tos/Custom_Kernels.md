---
title: Custom Kernels
category: How To Guides
categoryindex: 2
index: 1
---

# How to add your own custom filter kernels

Extending the library is one of the main features of the project.
It is as easy as modifying several files. Though, adding custom pixel manipulation logic could prove some challenge.
We will take a look at how to add custom kernels that do not involve an advanced logic.

## Requirements

Make sure the following requirements are installed on your system:

- [dotnet SDK](https://dotnet.microsoft.com/en-us/download/dotnet/7.0) 7.0 or higher
- OpenCL-compatible device with respective driver installed

## General project structure

By default the project's structure looks like this:

```ignorelang
ImageProcessing
├── ...
├── Agent.fs
├── FilterKernel.fs
├── GPUDevice.fs
├── GPUKernel.fs
├── ImageProcessing.fs
├── Logging.fs
├── Transformation.fs
├── RunStrategy.fs
├── Streaming.fs
├── Main.fs
└── ...
```

## Custom filter kernels for CPU and GPU

To add your own filters you will have to modify `FilterKernel.fs` as well as `Transformation.fs` files.

Let's add identity filter that takes an original input and outputs it without any modifications to pixel data.

1) Define a new kernel matrix inside a `FilterKernel.fs`. Let's call it `identitykernel`:

```fsharp
let identityKernel =
    [| [| 1; 0; 0; 0; 0 |]
       [| 0; 1; 0; 0; 0 |]
       [| 0; 0; 1; 0; 0 |]
       [| 0; 0; 0; 1; 0 |]
       [| 0; 0; 0; 0; 1 |] |]
    |> Array.map (Array.map float32)
    |> array2D
```

2) Extend the `Transformation` type inside the `Transformation.fs` by appending a new union case. Let us add `Identity` union case to it:

```fsharp
type Transformation =
    | ...
    | Identity
```

3) Lastly, define a parsing rule inside the same `Transformation.fs` that will parse a console input to a function to be applied to a pixel data.
For that you will have to modify `getTsfCPU`:

```fsharp
let getTsfCPU threads =
    function
    | ...
    | Identity -> CPU.applyTransform threads (EditType.Transformation identityKernel)
```

and `getTsfGPU` functions:

```fsharp
let getTsfGPU (clContext: ClContext) localWorkSize =
    function
    | ...
    | Identity -> GPU.applyTransform clContext localWorkSize (EditType.Transformation identityKernel)
```

And that is all. Now `identity` can be used as an argument in a console.

Adding your own transformations that do not involve kernel matrices is an advanced topic.
You can take a look at `ImageProcessing.fs` inside your project.
Several transformations are already defined there.

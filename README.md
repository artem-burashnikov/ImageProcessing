# ImageProcessing
[![Build status](https://github.com/artem-burashnikov/ImageProcessing/actions/workflows/build.yml/badge.svg)](https://github.com/artem-burashnikov/ImageProcessing/actions/workflows/build.yml?branch=main)
[![API docs](https://img.shields.io/badge/Documentation-API-yellowgreen)](https://artem-burashnikov.github.io/ImageProcessing/)
[![License: MIT Licence](https://img.shields.io/badge/license-MIT-blue)](https://github.com/artem-burashnikov/ImageProcessing/blob/main/LICENSE.md)

### An open-source project for editing raster images.

ImageProcessing performs image editing on CPU as well as GPGPU. The tool is written in F# and is available for Linux, macOS, and Windows. It can be used as a standalone application, or as a library integrated into other software.

### Features and capabilities

- Customizable multi-threaded and parallel computation support
- Application of transformations and filters on GPU (Nvidia, AMD, Intel)
- Several transformations and filters are available from the box (blur filter, edges detection, rotation, flip, etc.)
- Ability to add your own custom filters and transformations
- Ability to specify multiple filters and/or transformations to be applied
- Processing of multiple files in a specified directory
- Ready-to-use console application
- API documentation for developers

### Why use ImageProcessing?

GPGPU support and ability to add custom transformations makes it fast and flexible image editing software on .NET

### Platforms

- Windows (tested on Windows 10, 11)
- macOS (tested on ...)
- Linux (tested on Ubuntu 20.04)

### Third-party projects

* [Brahma.FSharp](https://github.com/YaccConstructor/Brahma.FSharp) F# to OpenCL translator for utilizing GPGPUs
* [SixLabors.ImageSharp](https://github.com/SixLabors/ImageSharp) utilities for loading, writing and saving images

### Documentation

Detailed [documentation](https://artem-burashnikov.github.io/ImageProcessing/) for the ImageProcessing API is available.

## Installation

You can install a package and its dependencies from GitHub following [these](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-nuget-registry#installing-a-package) steps or build an application from source.


### Manual build

Make sure the following **requirements** are installed on your system:

- [dotnet SDK](https://dotnet.microsoft.com/en-us/download/dotnet/7.0) 7.0 or higher
- OpenCL-compatible device with respective driver

To clone ImageProcessing locally, run the following git commands:

```bash
git clone https://github.com/artem-burashnikov/ImageProcessing; cd ImageProcessing
```

If you are on Windows, run:

```bash
build.cmd
```

On Linux and macOS run:

```bash
./build.sh
```

For optional build targets look at [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold). The template has been used in the project.

## How to use

```
USAGE: ImageProcessing [--help] --strategy <cpu|gpu|async1cpu|async2cpu|async1gpu|async2gpu> [--threads <uint>] -i <string>
                       -o <string> <blur|edges|highpass|laplacian|sobelv|rotate|rotateccw|reflecth|reflectv>...

TRANSFORMATIONS:

    <blur|edges|highpass|laplacian|sobelv|rotate|rotateccw|reflecth|reflectv>...
                          Provide transformations to be applied.

OPTIONS:

    --strategy <cpu|gpu|async1cpu|async2cpu|async1gpu|async2gpu>
                          Specify the run strategy to use
    --threads <uint>      Optionally provide a number of threads (a positive integer) to use for certain run strategies
    -i <string>           Input path: specify a path to an image file or to a folder containing images
    -o <string>           Output path: give a path to a folder
    --help                display this list of options.
```

### Running options

- `cpu` - Apply transformations on CPU.
- `gpu` - Apply transformations on GPU.
- `async1cpu`, `async1gpu` - Apply transformations on CPU or GPU utilizing multi-threading using agent-based parallel algorithm (delegation pipeline).
- `async2cpu`, `async2gpu` - Apply transformations on CPU or GPU utilizing multi-threading using agent-based parallel algorithm (splitting wokrload).
- `threads` - Optionally provide a number of threads to be utilized for corresponding run strategy.
- `i` - Input path. A directory or an image file.
- `o` - Output directory.

### List of built-in filters and transformations

- `blur` - Gaussian blur.
- `edges` - A simple edge detection filter.
- `highpass` - Highpass image filter.
- `laplacian` - Laplacian edge detection algorithm.
- `sobelv` - Vertical sobel filter.
- `rotate` - Perform a clockwise 90 degrees rotation.
- `rotateccw` - Perform a counterclockwise 90 degrees rotation.
- `reflecth` - Horizontally reflect an image.
- `reflectv` - Vertically reflect an image.

## Examples

Applying a horizontal reflect on an image using GPU:

```
./ImageProcessing --strategy gpu -i input/image.jpg -o output/ reflecth
```

Sequentially applying blur and clockwise rotation on an image using CPU:

```
./ImageProcessing --strategy cpu -i input/image.jpg -o output/ blur rotate
```

Processing multiple files inside a directory and applying blur, edges and vertical reflect using CPU and utilizing multiple threads:

```
./ImageProcessing --strategy async2cpu --threads 4 -i input/ -o output/ blur edges reflectv
```

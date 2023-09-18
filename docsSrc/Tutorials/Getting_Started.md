---
title: Getting Started
category: Tutorials
categoryindex: 1
index: 1
---

# Getting Started

This section will guide you through installing and using the application.

## Install from the command line:

Navigate to the root of your project and run:

```bash
dotnet add package {{fsdocs-collection-name}}.{{fsdocs-suffix}} --version {{fsdocs-package-version}}
```

## Usage

CLI is available:

```ignorelang
ImageProcessing [--help] --strategy <cpu|gpu|async1cpu|async2cpu|async1gpu|async2gpu> [--threads <uint>] -i <string>
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
    --help                      display this list of options.
```

## Examples

The following examples assume you are have `input` and `output` directories in the root of your project.

Applying a horizontal reflect on an image using GPU:

```
dotnet run ImageProcessing --strategy gpu -i input/image.jpg -o output/ reflecth
```

Sequentially applying blur and clockwise rotation on an image using CPU:

```
dotnet run ImageProcessing --strategy cpu -i input/image.jpg -o output/ blur rotate
```

Processing multiple files inside a directory and applying blur, edges and vertical reflect using CPU and utilizing multiple threads:

```
dotnet run ImageProcessing --strategy async2cpu --threads 4 -i input/ -o output/ blur edges reflectv
```

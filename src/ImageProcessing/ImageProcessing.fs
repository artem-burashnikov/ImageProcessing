/// <summary>
/// Provides image processing utilities and types.
/// </summary>
module ImageProcessing.ImageProcessing

open System
open Brahma.FSharp
open Microsoft.FSharp.Core
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

/// <summary>
/// Contains helper types and functions for image editing.
/// </summary>
module HelpProviders =

    /// <summary>
    /// Represents the direction of rotation (clockwise or counterclockwise).
    /// </summary>
    type RotationDirection =
        | Clockwise
        | Counterclockwise

    /// <summary>
    /// Represents the direction of reflection (horizontal or vertical).
    /// </summary>
    type ReflectionDirection =
        | Horizontal
        | Vertical

    /// <summary>
    /// Represents various types of image editing operations.
    /// </summary>
    [<RequireQualifiedAccess>]
    type EditType =
        | Transformation of float32[,]
        | Rotation of RotationDirection
        | Reflection of ReflectionDirection

    /// <summary>
    /// Represents a virtual array that provides memory bounds checking.
    /// </summary>
    type VirtualArray<'A>(memory: array<'A>, head: int, length: int) =
        // When an instance is created, check that it is within the specified memory limits
        do
            if head + length - 1 >= memory.Length then
                failwith
                    $"Failed to allocate required memory: %A{length} for VirtualArray at the specified starting index: %A{head}"

        /// <summary>
        /// Gets the underlying memory array.
        /// </summary>
        member this.Memory = memory

        /// <summary>
        /// Gets the starting index within the memory array.
        /// </summary>
        member this.Head = head

        /// <summary>
        /// Gets the length of the virtual array.
        /// </summary>
        member this.Length = length

        /// <summary>
        /// Gets a value indicating whether the virtual array is empty.
        /// </summary>
        member this.IsEmpty = length = 0

        /// <summary>
        /// Gets or sets the value at the specified index in the virtual array.
        /// </summary>
        /// <returns>The value at the specified index.</returns>
        /// <exception cref="System.IndexOutOfRangeException">Thrown if the index is out of bounds.</exception>
        member this.Item
            with get i =
                if head + i >= memory.Length then
                    failwith "VirtualArray.get: Index out of bounds of the general memory"
                else
                    this.Memory[this.Head + i]
            and set i value =
                if head + i >= memory.Length then
                    failwith "VirtualArray.set: Index out of bounds of the general memory"
                else
                    this.Memory[ this.Head + i ] <- value

        /// <summary>
        /// Splits the virtual array into a specified number of sub-arrays.
        /// </summary>
        /// <param name="count">The number of sub-arrays to create.</param>
        /// <param name="vArray">The initial array to be split.</param>
        /// <returns>An array of virtual sub-arrays.</returns>
        /// <exception cref="System.ArgumentException">Thrown if 'count' is not positive.</exception>
        static member splitInto count (vArray: VirtualArray<'A>) =
            if count <= 0 then
                failwith $"VirtualArray.SplitIntoCount count argument: {count} must be positive"

            let len = vArray.Length

            if len = 0 then
                [||]
            else
                let count = min count len
                let res = Array.zeroCreate count
                let minChunkSize = len / count
                let mutable startIndex = vArray.Head

                for i in 0 .. len % count - 1 do
                    res[i] <- VirtualArray(vArray.Memory, startIndex, minChunkSize + 1)
                    startIndex <- startIndex + minChunkSize + 1

                for i in len % count .. count - 1 do
                    res[i] <- VirtualArray(vArray.Memory, startIndex, minChunkSize)
                    startIndex <- startIndex + minChunkSize

                res

        /// <summary>
        /// Creates a new virtual array that mirrors an existing array.
        /// </summary>
        /// <param name="arr">The array to mirror.</param>
        /// <returns>A virtual array mirroring 'arr'.</returns>
        static member mirror(arr: array<'A>) = VirtualArray(arr, 0, arr.Length)

        /// <summary>
        /// Folds two virtual arrays using a folder function.
        /// </summary>
        /// <param name="folder">The folder function.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="vArray1">The first virtual array.</param>
        /// <param name="vArray2">The second virtual array.</param>
        /// <returns>The folded result.</returns>
        /// <exception cref="System.ArgumentException">Thrown if 'vArray1' and 'vArray2' have different lengths.</exception>
        static member fold2 folder (state: 'State) (vArray1: VirtualArray<'A>) (vArray2: VirtualArray<'B>) =
            if vArray1.Length <> vArray2.Length then
                failwith $"Invalid argument vArray1.Length: %A{vArray1.Length} vArray2.Length: %A{vArray2.Length}"

            let mutable state = state

            for i in 0 .. vArray1.Length - 1 do
                state <- folder state vArray1[i] vArray2[i]

            state

        /// <summary>
        /// Iterates over the virtual array and applies an action to each element.
        /// </summary>
        /// <param name="action">The action to apply to each element.</param>
        /// <param name="vArray">The input array</param>
        /// <param name="output">The output array to store results.</param>
        /// <exception cref="System.ArgumentException">Thrown if 'output' does not match the virtual array length.</exception>
        static member iteri2 action (vArray: VirtualArray<'A>) (output: array<'A>) =
            for i in 0 .. vArray.Length - 1 do
                output[vArray.Head + i] <- action (vArray.Head + i) vArray.Memory[vArray.Head + i]


    /// <summary>
    /// Represents an image with pixel data.
    /// </summary>
    [<Struct>]
    type Image =
        val Data: array<byte>
        val VirtualData: VirtualArray<byte>
        val Width: int
        val Height: int
        val Name: string

        new(data, virtualData, width, height, name) =
            { Data = data
              VirtualData = virtualData
              Width = width
              Height = height
              Name = name }

        new(data, width, height, name) =
            { Data = data
              VirtualData = VirtualArray.mirror data
              Width = width
              Height = height
              Name = name }

    /// <summary>
    /// Flattens a 2D array into a 1D array.
    /// </summary>
    /// <param name="array2D">The 2D array to flatten.</param>
    /// <returns>A flattened 1D array.</returns>
    let flattenArray2D array2D =
        [| for x in 0 .. (Array2D.length1 array2D) - 1 do
               for y in 0 .. (Array2D.length2 array2D) - 1 do

                   yield array2D[x, y] |]


    /// <summary>
    /// Loads an image from a file.
    /// </summary>
    /// <param name="file">The path to the image file.</param>
    /// <returns>The loaded image.</returns>
    let loadAsImage (file: string) =
        let img = Image.Load<L8> file
        let buf = Array.zeroCreate<byte> (img.Width * img.Height)
        img.CopyPixelDataTo(Span<byte> buf)
        Image(buf, img.Width, img.Height, System.IO.Path.GetFileName file)

    /// <summary>
    /// Saves an image to a file.
    /// </summary>
    /// <param name="image">The image to save.</param>
    /// <param name="file">The path to the output file.</param>
    let saveImage (image: Image) file =
        let img = Image.LoadPixelData<L8>(image.Data, image.Width, image.Height)
        img.Save file


/// <summary>
/// Contains functions for applying image transformations using CPU resources.
/// </summary>
module CPU =

    open HelpProviders

    /// <summary>
    /// Applies a given image transformation using CPU resources.
    /// </summary>
    /// <param name="threads">The number of CPU threads to use.</param>
    /// <param name="parameter">The type of transformation to apply.</param>
    /// <param name="img">The input image.</param>
    /// <returns>The transformed image.</returns>
    let applyTransform threads parameter (img: Image) =

        if threads <= 0 then
            failwith "Number of threads has to be positive"

        let threads = Convert.ToInt32 threads

        // Store image dimensions, because we might need to swap them later if Rotation is performed
        let mutable width = img.Width
        let mutable height = img.Height

        // The resulting pixel data depends on the matching case of the transformation parameter
        let result =
            match parameter with
            | EditType.Rotation rotationDirection ->
                /// Function performs a pixel remapping logic to rotate an image
                let remapPixels (input: VirtualArray<byte>) (output: array<byte>) =
                    // Going through each of VirtualArray's indices...
                    for i in input.Head .. input.Head + input.Length - 1 do

                        // ... compute their global memory indices...
                        let pi = i / img.Width
                        let pj = i % img.Width

                        // ... and perform remapping on global memory
                        match rotationDirection with
                        | Clockwise ->
                            output[pj * img.Height + img.Height - pi - 1] <- input.Memory[pi * img.Width + pj]
                        | Counterclockwise ->
                            output[(img.Width - pj - 1) * img.Height + pi] <- input.Memory[pi * img.Width + pj]

                // We create a Virtual and an output buffer array out of a given image's data
                let input = VirtualArray(img.Data, 0, img.Height * img.Width)
                let output = Array.zeroCreate (width * height)

                if threads = 1 then
                    // For non-async computations on the main thread we just pass the whole VirtualArray
                    remapPixels input output
                else
                    // For async computations we split a given data between processors.
                    // Each performs its own pixel remapping logic.
                    let input = VirtualArray.splitInto threads input
                    Array.Parallel.iter (fun vArray -> remapPixels vArray output) input

                // Swap dimensions
                width <- height
                height <- img.Width

                // Bind output to the result
                output

            | EditType.Reflection reflectionDirection ->
                /// Function performs a pixel remapping logic to reflect an image
                let remapPixels (input: VirtualArray<byte>) (output: array<byte>) =
                    // Going through each of VirtualArray's indices...
                    for i in input.Head .. input.Head + input.Length - 1 do

                        // ... compute their global memory indices...
                        let pi = i / img.Width
                        let pj = i % img.Width

                        // ... and perform remapping on global memory
                        match reflectionDirection with
                        | Horizontal ->
                            // nw <- sw
                            output[pi * img.Width + pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + pj]
                            // nw -> sw
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + pj] <-
                                input.Memory[pi * img.Width + pj]
                            // ne <- se
                            output[pi * img.Width + img.Width - 1 - pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj]
                            // ne -> se
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj] <-
                                input.Memory[pi * img.Width + img.Width - 1 - pj]

                        | Vertical ->
                            // nw <- ne
                            output[pi * img.Width + pj] <- input.Memory[pi * img.Width + img.Width - 1 - pj]
                            // nw -> ne
                            output[pi * img.Width + img.Width - 1 - pj] <- input.Memory[pi * img.Width + pj]
                            // sw <- se
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj]
                            // sw -> se
                            output[(img.Height - 1) * img.Width - (pi * img.Width) + img.Width - 1 - pj] <-
                                input.Memory[(img.Height - 1) * img.Width - (pi * img.Width) + pj]

                // We create a Virtual array and an output buffer out of a given image's data
                let input = VirtualArray(img.Data, 0, img.Height * img.Width)
                let output = Array.zeroCreate (img.Height * img.Width)

                if threads = 1 then
                    // For non-async computations on the main thread we just pass the whole VirtualArray
                    remapPixels input output
                else
                    // For async computations we split a given data between processors.
                    // Each performs its own pixel remapping logic.
                    let input = VirtualArray.splitInto threads input
                    Array.Parallel.iter (fun vArray -> remapPixels vArray output) input

                // Bind output to the result
                output

            | EditType.Transformation filter ->

                // Filter parameters
                let filterD = (Array2D.length1 filter) / 2

                // Flatten 2dArray of pixels
                let filter = flattenArray2D filter

                // Pixel processing logic
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

                    // Returns weighted sum of pixels
                    Array.fold2 (fun s x y -> s + x * y) 0.0f filter dataToHandle

                // Create a Virtual array and an output buffer out of a given image's data
                let input = VirtualArray(img.Data, 0, img.Height * img.Width)
                let output = Array.zeroCreate (width * height)

                if threads = 1 then
                    // For non-async computations on the main thread we just pass the whole VirtualArray
                    VirtualArray.iteri2 (fun i _ -> byte (processPixel i)) input output
                    output

                else
                    // For async computations we split a given data between processors.
                    // Each performs its own pixel remapping logic.
                    let input = VirtualArray.splitInto threads input
                    let action = VirtualArray.iteri2 (fun i _ -> byte (processPixel i))
                    Array.Parallel.iter (fun (vArray: VirtualArray<byte>) -> action vArray output) input
                    output

        // Return the final result
        Image(result, width, height, img.Name)


/// <summary>
/// Contains functions for applying image transformations using GPU resources.
/// </summary>
module GPU =

    open HelpProviders

    /// <summary>
    /// Applies a given image transformation using GPU resources.
    /// </summary>
    /// <param name="clContext">The OpenCL context for GPU operations.</param>
    /// <param name="localWorkSize">The size of the local workgroup.</param>
    /// <returns>A function that applies the specified transformation to an image.</returns>
    let applyTransform (clContext: ClContext) localWorkSize =

        let queue = clContext.QueueProvider.CreateQueue()

        // The lambda-function will be returned
        fun (parameter: EditType) (img: Image) ->

            // Store image dimensions, because we might need to swap them later if Rotation is performed
            let mutable width = img.Width
            let mutable height = img.Height

            let input = clContext.CreateClArray<_>(img.Data, HostAccessMode.NotAccessible)

            let output =
                clContext.CreateClArray(
                    img.Data.Length,
                    HostAccessMode.NotAccessible,
                    allocationMode = AllocationMode.Default
                )

            let (output: ClArray<byte>) =
                match parameter with
                | EditType.Rotation rotationDirection ->
                    let kernel =
                        match rotationDirection with
                        | Clockwise -> GPUKernel.rotation clContext localWorkSize 1
                        | Counterclockwise -> GPUKernel.rotation clContext localWorkSize 0

                    // Out of scope assignment for correct image dimensions when saving rotated images
                    height <- img.Width
                    width <- img.Height

                    kernel queue input img.Height img.Width output

                | EditType.Reflection reflectionDirection ->
                    let kernel =
                        match reflectionDirection with
                        | Horizontal -> GPUKernel.reflection clContext localWorkSize 1
                        | Vertical -> GPUKernel.reflection clContext localWorkSize 0

                    kernel queue input img.Height img.Width output

                | EditType.Transformation filter ->
                    let filterD = (Array2D.length1 filter) / 2
                    let filter = flattenArray2D filter

                    let clFilter =
                        clContext.CreateClArray<_>(filter, HostAccessMode.NotAccessible, DeviceAccessMode.ReadOnly)

                    let kernel = GPUKernel.filter clContext localWorkSize
                    let res = kernel queue clFilter filterD input img.Height img.Width output
                    // Memory clean-up on GPU
                    queue.Post(Msg.CreateFreeMsg clFilter)
                    res

            let result = Array.zeroCreate (img.Height * img.Width)

            let result = queue.PostAndReply(fun ch -> Msg.CreateToHostMsg(output, result, ch))

            // Memory clean-up on GPU
            queue.Post(Msg.CreateFreeMsg input)
            queue.Post(Msg.CreateFreeMsg output)

            Image(result, width, height, img.Name)

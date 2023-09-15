namespace ImageProcessing

open System.IO
open Argu
open ImageProcessing.Transformation
open ImageProcessing.RunStrategy

/// <summary>
/// The main module of the ImageProcessing application.
/// </summary>
module Main =

    /// <summary>
    /// Represents the command-line arguments for the application.
    /// </summary>
    type Arguments =
        | [<MainCommand; Mandatory>] Transformations of Transformation list
        | [<Mandatory>] Strategy of RunStrategy
        | Threads of uint
        | [<Mandatory; CustomCommandLine("-i")>] Input of string
        | [<Mandatory; CustomCommandLine("-o")>] Output of string

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Transformations _ -> "Provide transformations to be applied."
                | Strategy _ -> "Specify the run strategy to use"
                | Threads _ ->
                    "Optionally provide a number of threads (a positive integer) to use for certain run strategies"
                | Input _ -> "Input path: specify a path to an image file or to a folder containing images"
                | Output _ -> "Output path: give a path to a folder"


    /// <summary>
    /// Represents the input path for image processing.
    /// </summary>
    [<RequireQualifiedAccess>]
    type InputPath =
        /// <summary>
        /// Specifies that the input is a single image file.
        /// </summary>
        | File of string
        /// <summary>
        /// Specifies that the input is a folder containing image files.
        /// </summary>
        | Folder of string
        /// <summary>
        /// Specifies that the input is a file, but it does not have a supported image format.
        /// </summary>
        | NoImgFile of string
        /// <summary>
        /// Specifies that the input path was not found.
        /// </summary>
        | NotFound of string
        /// <summary>
        /// Specifies that the input path was not provided.
        /// </summary>
        | Unspecified

    /// <summary>
    /// Represents the output path for image processing.
    /// </summary>
    [<RequireQualifiedAccess>]
    type OutputPath =
        /// <summary>
        /// Specifies that the output should be a folder where processed images will be saved.
        /// </summary>
        | Folder of string
        /// <summary>
        /// Specifies that the output path was not found.
        /// </summary>
        | NotFound of string
        /// <summary>
        /// Specifies that the output path was not provided.
        /// </summary>
        | Unspecified

    let extensions =
        set
            [| ".gif"
               ".jpg"
               ".jpeg"
               ".bmp"
               ".pbm"
               ".png"
               ".tif"
               ".tiff"
               ".tga"
               ".webp" |]

    /// <summary>
    /// Checks if a file has an image extension.
    /// </summary>
    /// <param name="file">The file path to check.</param>
    /// <returns>True if the file has an image extension; otherwise, false.</returns>
    let isImg (file: string) =
        Set.contains (Path.GetExtension file) extensions

    /// <summary>
    /// Runs the image processing based on the provided arguments.
    /// </summary>
    /// <param name="inputPath">The input path for image files.</param>
    /// <param name="outputPath">The output path for processed images.</param>
    /// <param name="transformations">The list of transformations to apply to the images.</param>
    /// <param name="strategy">The run strategy to use.</param>
    /// <param name="threads">The number of threads or agents to use for processing.</param>
    /// <returns>An exit code.</returns>
    let runEditImage
        (inputPath: InputPath)
        (outputPath: OutputPath)
        (transformations: Transformation list)
        (strategy: RunStrategy)
        threads
        =

        match inputPath, outputPath with
        | InputPath.NoImgFile sIn, _ ->
            eprintfn $"Input {Path.GetFileName sIn} is unsupported file type"
            1
        | InputPath.NotFound sIn, _ ->
            eprintfn $"Input path {sIn} not found"
            1
        | InputPath.Unspecified, _ ->
            eprintfn "No input path provided. Call with --help for usage information."
            1
        | _, OutputPath.NotFound sOut ->
            eprintfn $"Output path {sOut} not found"
            1
        | _, OutputPath.Unspecified ->
            eprintfn "No output path provided. Call with --help for usage information."
            1
        | InputPath.File sIn, OutputPath.Folder sOut ->
            if isImg sIn then
                let imgFiles = [ sIn ]
                Streaming.processAllFiles strategy threads imgFiles sOut transformations
                0
            else
                eprintfn $"Provided file {Path.GetFileName sIn} is not an image file."
                1
        | InputPath.Folder sIn, OutputPath.Folder sOut ->
            let imgFiles = Streaming.listAllFiles sIn |> Seq.filter isImg

            if Seq.isEmpty imgFiles then
                eprintfn "No image files found in the specified folder."
                1
            else
                Streaming.processAllFiles strategy threads imgFiles sOut transformations
                0

    /// <summary>
    /// The entry point of the application.
    /// </summary>
    /// <param name="argv">The command-line arguments.</param>
    [<EntryPoint>]
    let main (argv: string array) =

        let errorHandler =
            ProcessExiter(
                colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some System.ConsoleColor.Blue
            )

        let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)

        let results = parser.ParseCommandLine argv

        let inputPath =
            match results.TryGetResult <@ Arguments.Input @> with
            | Some input ->
                if Directory.Exists input then
                    InputPath.Folder input
                elif File.Exists input && isImg input then
                    InputPath.File input
                elif File.Exists input then
                    InputPath.NoImgFile input
                else
                    InputPath.NotFound input
            | None -> InputPath.Unspecified

        let outputPath =
            match results.TryGetResult <@ Arguments.Output @> with
            | Some output ->
                if Directory.Exists output then
                    OutputPath.Folder output
                else
                    OutputPath.NotFound output
            | None -> OutputPath.Unspecified

        let transformations = results.GetResult(Transformations)

        let strategy = results.GetResult(Strategy)

        let threads =
            let maybeThreads = results.TryGetResult(Threads)

            match maybeThreads with
            | Some count -> count
            | None -> 1u

        if threads = 0u then
            eprintfn "Number of threads cannot be 0"
            1
        else
            runEditImage inputPath outputPath transformations strategy threads |> exit

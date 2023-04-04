namespace ImageProcessing

open System.IO
open Argu
open ImageProcessing.Transformation
open ImageProcessing.RunStrategy

module Main =

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


    [<RequireQualifiedAccess>]
    type InputPath =
        | File of string
        | Folder of string
        | NoImgFile of string
        | NotFound of string
        | Unspecified

    [<RequireQualifiedAccess>]
    type OutputPath =
        | Folder of string
        | NotFound of string
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

    let isImg (file: string) =
        Set.contains (Path.GetExtension file) extensions

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
            | Some count -> System.Convert.ToInt32 count
            | None -> 1

        if threads = 0 then
            eprintfn "Number of threads cannot be 0"
            1
        else
            runEditImage inputPath outputPath transformations strategy threads |> exit

namespace ImageProcessing

open System.IO
open Argu
open ImageProcessing.Transformations
open ImageProcessing.RunStrategy

module Main =

    type Arguments =
        | [<MainCommand; Mandatory>] Transformations of Transformation list
        | [<Mandatory>] Input of string
        | [<Mandatory>] Output of string
        | [<Mandatory>] Strategy of RunStrategy

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Transformations _ -> "Provide transformations to be applied."
                | Input _ -> "Input path: specify a path to an image file or to a folder containing images"
                | Output _ -> "Output path: give a path to a folder"
                | Strategy _ -> "Specify the run strategy to use"

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
        (editor: Transformation list)
        (strategy: RunStrategy)
        =

        match inputPath, outputPath, editor, strategy with
        | InputPath.NoImgFile sIn, _, _, _ ->
            eprintfn $"Input {Path.GetFileName sIn} is unsupported file type"
            1
        | InputPath.NotFound sIn, _, _, _ ->
            eprintfn $"Input path {sIn} not found"
            1
        | InputPath.Unspecified, _, _, _ ->
            eprintfn "No input path provided. Call with --help for usage information."
            1
        | _, OutputPath.NotFound sOut, _, _ ->
            eprintfn $"Output path {sOut} not found"
            1
        | _, OutputPath.Unspecified, _, _ ->
            eprintfn "No output path provided. Call with --help for usage information."
            1
        | InputPath.File sIn, OutputPath.Folder sOut, transformations, strategy ->
            if isImg sIn then
                let transformations = List.map getTransformation transformations
                let imgFiles = [ sIn ]

                match strategy with
                | CPU ->
                    Streaming.processAllFilesNaiveCPU imgFiles sOut transformations
                    0
                | AsyncCPU1 ->
                    Streaming.processAllFilesAgents1 imgFiles sOut transformations
                    0
                | AsyncCPU2 ->
                    eprintfn "Not yet implemented"
                    1
                | GPU ->
                    eprintfn "Not yet implemented"
                    1
            else
                eprintf $"Provided file {Path.GetFileName sIn} is not an image file."
                1
        | InputPath.Folder sIn, OutputPath.Folder sOut, transformations, strategy ->
            let imgFiles = Streaming.listAllFiles sIn |> Seq.filter isImg

            if Seq.isEmpty imgFiles then
                eprintf "No image files found in the specified folder."
                1
            else
                let transformations = List.map getTransformation transformations

                match strategy with
                | CPU ->
                    Streaming.processAllFilesNaiveCPU imgFiles sOut transformations
                    0
                | AsyncCPU1 ->
                    Streaming.processAllFilesAgents1 imgFiles sOut transformations
                    0
                | AsyncCPU2 ->
                    eprintfn "Not yet implemented"
                    1
                | GPU ->
                    eprintfn "Not yet implemented"
                    1

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

        let editor = results.GetResult(Transformations)

        let strategy = results.GetResult(Strategy)

        runEditImage inputPath outputPath editor strategy |> exit

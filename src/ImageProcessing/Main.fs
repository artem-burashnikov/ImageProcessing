namespace ImageProcessing

open System.IO
open Argu
open Applicators

module Main =

    type Arguments =
        | [<MainCommand; Mandatory>] Applicators of string list
        | [<Mandatory>] Input of string
        | [<Mandatory>] Output of string

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Applicators _ ->
                    "Provide applicators to be used. Available applicators: blur, edges, highpass, laplacian, sobelv, rotate, rotateccw"
                | Input _ -> "Input path: specify a path to a folder containing images"
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
        set [| ".gif"; ".jpg"; ".jpeg"; ".bmp"; ".pbm"; ".png"; ".tiff"; ".tga"; ".webp" |]

    let isImg (file: string) =
        Set.contains (Path.GetExtension file) extensions

    let parseToApplicator lst =
        List.map Applicator.ApplicatorFromStr lst

    let runEditImageOnCPU (applicators: Applicator list) (inputPath: InputPath) (outputPath: OutputPath) =

        match inputPath, outputPath, applicators with
        | InputPath.NoImgFile sIn, _, _ ->
            eprintfn $"Input {sIn} is unsupported file type"
            1
        | InputPath.NotFound sIn, _, _ ->
            eprintfn $"Input path {sIn} not found"
            1
        | InputPath.Unspecified, _, _ ->
            eprintfn "No input path provided. Call with --help for usage information."
            1
        | _, OutputPath.NotFound sOut, _ ->
            eprintfn $"Output path {sOut} not found"
            1
        | _, OutputPath.Unspecified, _ ->
            eprintfn "No output path provided. Call with --help for usage information."
            1
        | _, _, [] ->
            eprintf "No applicators provided. Call with --help for usage information."
            1
        | InputPath.File sIn, OutputPath.Folder sOut, applicators ->
            if isImg sIn then
                let applicators = List.map getApplicator applicators
                let imgFiles = [ sIn ]
                Streaming.processAllFilesNaiveCPU imgFiles sOut applicators
                0
            else
                eprintf $"Provided file {Path.GetFileName sIn} is not an image file."
                1
        | InputPath.Folder sIn, OutputPath.Folder sOut, applicators ->
            let imgFiles = Streaming.listAllFiles sIn |> Seq.filter isImg

            if Seq.isEmpty imgFiles then
                eprintf "No image files found in the specified folder."
                1
            else
                let applicators = List.map getApplicator applicators
                Streaming.processAllFilesNaiveCPU imgFiles sOut applicators
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

        let applicators =
            match results.TryGetResult <@ Arguments.Applicators @> with
            | Some applicators ->
                let applicatorsList =
                    parseToApplicator applicators |> List.filter (fun f -> f <> Applicator.Invalid)

                match applicatorsList with
                | [] -> failwith "Could not find any applicators from a provided list of applicators."
                | _ -> applicatorsList
            | None -> [ Applicator.Invalid ]

        runEditImageOnCPU applicators inputPath outputPath |> exit

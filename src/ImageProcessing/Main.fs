namespace ImageProcessing

open System.IO
open Argu.ArguAttributes
open Brahma.FSharp
open Argu
open FilterApplicators

module Main =

    type Arguments =
        | [<MainCommand; Mandatory>] Filters of string list
        | [<Mandatory>] Input of string
        | [<Mandatory>] Output of string

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Filters _ ->
                    "Provide filter names. Available filters: blur, edges, highpass, laplacian, sobel, rotate, rotatecww"
                | Input _ -> "Input path: specify a path to a folder containing images"
                | Output _ -> "Output path: give a path to a folder"

    [<RequireQualifiedAccess>]
    type InputPath =
        | Folder of string
        | NoImgFile of string
        | NotFound of string
        | Unspecified

    [<RequireQualifiedAccess>]
    type OutputPath =
        | Folder of string
        | NotFound of string
        | Unspecified

    let rec parseToFilter lst = List.map Filter.FilterFromStr lst

    let runImageFilterOnCPU (filterApplicators: Filter list) (inputPath: InputPath) (outputPath: OutputPath) =

        match inputPath, outputPath, filterApplicators with
        | InputPath.NoImgFile sIn, _, _ ->
            eprintfn $"Input path {sIn} is unsupported file type"
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
            eprintf "No filters provided. Call with --help for usage information."
            1
        | InputPath.Folder sIn, OutputPath.Folder sOut, applicators ->
            let invalidFilter = Seq.tryFind (fun f -> f = Filter.Invalid) applicators

            match invalidFilter with
            | Some f ->
                eprintf $"Invalid filter {f} provided. Call with --help for usage information."
                1
            | None ->
                let applicators = List.map getApplicator applicators
                Streaming.processAllFilesNaiveCPU sIn sOut applicators
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

        let applicatorsList =
            match results.TryGetResult <@ Arguments.Filters @> with
            | Some applicators -> parseToFilter applicators
            | None -> [ Filter.Invalid ]



        0

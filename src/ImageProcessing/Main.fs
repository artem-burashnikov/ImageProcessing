namespace ImageProcessing

open Brahma.FSharp
open Argu

module Main =

    let extensions =
        set [| ".gif"; ".jpg"; ".jpeg"; ".bmp"; ".pbm"; ".png"; ".tiff"; ".tga"; ".webp" |]

    type Arguments =
        | Test

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Test -> "Testing"

    [<RequireQualifiedAccess>]
    type InputPath =
        | File of string
        | Folder of string
        | Multiple of files: string list * folder: string list
        | NoImgFile of string
        | NotFound of string
        | Unspecified

    [<RequireQualifiedAccess>]
    type OutputPath =
        | IO of string
        | NotKnown

    let isImg (file: string) =
        Set.contains (System.IO.Path.GetExtension file) extensions

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

        0

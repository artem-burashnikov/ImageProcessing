namespace ImageProcessing

open Brahma.FSharp
open Argu

module Main =

    let extensions = set [| ".gif"; ".jpg"; ".jpeg"; ".bmp"; ".pbm"; ".png"; ".tiff"; ".tga"; ".webp" |]

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
        0

namespace ImageProcessing

open Brahma.FSharp

module Main =
    let pathToExamples = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "images")
    let inputFolder = System.IO.Path.Combine(pathToExamples, "input")
    let outputFolder = System.IO.Path.Combine(pathToExamples, "output")


    [<EntryPoint>]
    let main (argv: string array) =

        0

open ImageProcessing
open Argu

type FilterType =
    | GaussianBlur
    | Edges
    | Identity
    | UnsharpMasking
    | Ridge
    | TopSobel
    | Outline
    | Sharpen


type CmdArgs =
    | [<Mandatory>] Input_File of string
    | [<Mandatory>] Out_File of string
    | [<Mandatory>] Filter_Type of filters: FilterType list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Input_File _ -> "File to process."
            | Out_File _ -> "Where to save result."
            | Filter_Type _ -> "Filters to apply. "


[<EntryPoint>]
let main argv =
    printfn "%A" argv
    let parser = ArgumentParser.Create<CmdArgs>(programName = "ImageProcessing")

    let usage = parser.PrintUsage()

    let results = parser.Parse argv

    let inFile = results.GetResult Input_File
    let outFile = results.GetResult Out_File
    let filters = results.GetResult Filter_Type

    let inImage = loadAs2DArray inFile

    let resultImage =
        let folder st =
            function
            | GaussianBlur -> applyFilter gaussianBlurKernel st
            | Edges -> applyFilter edgesKernel st
            | Identity -> applyFilter idKernel st
            | UnsharpMasking -> applyFilter unsharpMaskingKernel st
            | Ridge -> applyFilter ridgeKernel st
            | TopSobel -> applyFilter topSobelKernel st
            | Outline -> applyFilter outlineKernel st
            | Sharpen -> applyFilter sharpenKernel st

        filters |> List.fold folder inImage

    save2DByteArrayAsImage resultImage outFile
    0

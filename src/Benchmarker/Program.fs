module Program

open System
open System.Timers
open Learning
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open Perfolizer.Horology
open Perfolizer.Metrology
open BenchmarkDotNet.Exporters
open System.Globalization
open System.IO

let bigDataElemCount = [ 50000..10000..150000 ]
let smallDataElemCount = [ 5000..1000..15000 ]


type BigDataArrayAndListSortComparisonBenchmark() =
    let rand = Random()

    member public this._elemCounts = bigDataElemCount

    [<ParamsSource("_elemCounts")>]
    member val Element_Count = 0 with get, set

    member val dataList = [ 1 ] with get, set
    member val dataArray = [| 1 |] with get, set


    [<IterationSetup>]
    member this.initData() =
        this.dataList <- [ 1 .. this.Element_Count ] |> List.map (fun _ -> rand.Next(-100000, 100000))
        this.dataArray <- [| 1 .. this.Element_Count |] |> Array.map (fun _ -> rand.Next(-100000, 100000))

    [<Benchmark>]
    member this.List_Quick_Sort() = ListSorts.quickSort this.dataList

    [<Benchmark>]
    member this.List_Merge_Sort() = ListSorts.mergeSort this.dataList

    [<Benchmark>]
    member this.Array_Quick_Sort() = ArraySorts.quickSort this.dataArray

    [<Benchmark>]
    member this.Array_Merge_Sort() = ArraySorts.mergeSort this.dataArray

    [<Benchmark(Description = "List.sort")>]
    member this.ListSort() = List.sort this.dataList

    [<Benchmark(Baseline = true, Description = "Array.sort")>]
    member this.ArraySort() = Array.sort this.dataArray

type SmallDataListAndArraySortComparisonBenchmark() =
    let rand = Random()

    member public this._elemCounts = smallDataElemCount

    [<ParamsSource("_elemCounts")>]
    member val Element_Count = 0 with get, set

    member val dataList = [ 1 ] with get, set
    member val dataArray = [| 1 |] with get, set


    [<IterationSetup>]
    member this.initData() =
        this.dataList <- [ 1 .. this.Element_Count ] |> List.map (fun _ -> rand.Next(-100000, 100000))
        this.dataArray <- [| 1 .. this.Element_Count |] |> Array.map (fun _ -> rand.Next(-100000, 100000))

    [<Benchmark>]
    member this.List_Bubble_Sort() = ListSorts.bubbleSort this.dataList

    [<Benchmark>]
    member this.List_Insertion_Sort() = ListSorts.insertionSort this.dataList

    [<Benchmark(Baseline = true)>]
    member this.Array_Bubble_Sort() = ArraySorts.bubbleSort this.dataArray

    [<Benchmark>]
    member this.Array_Insertion_Sort() = ArraySorts.insertionSort this.dataArray


[<EntryPoint>]
let main args =
    let tablesPath = "./src/PythonPlotting/Tables/"
    let artifactsPath = "./BenchmarkDotNet.Artifacts/"


    let style =
        new SummaryStyle(
            cultureInfo = CultureInfo.InvariantCulture,
            printUnitsInHeader = true,
            printUnitsInContent = false,
            timeUnit = TimeUnit.Millisecond, // Seconds for slow sorts // Milliseconds for fast sorts
            sizeUnit = SizeUnit.MB
        )

    let exporter = Csv.CsvExporter(Csv.CsvSeparator.Semicolon, style)

    let config =
        ManualConfig
            .Create(DefaultConfig.Instance)
            .WithSummaryStyle(style)
            .AddExporter(exporter)


    let switcher =
        BenchmarkSwitcher
            [| typeof<BigDataArrayAndListSortComparisonBenchmark>
               typeof<SmallDataListAndArraySortComparisonBenchmark> |]

    let summaries = switcher.Run(args, config)

    for summary in summaries do
        let title = summary.Title
        let filename = title.Split('-')[0]
        let filepath = $"{artifactsPath}results/{filename}-report.csv"

        File.Copy(filepath, $"{tablesPath}{filename}.csv", true)

    0

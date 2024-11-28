module Program

open System
open System.Threading
open Learning
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running


[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, iterationCount = 1, warmupCount = 1)>]
type ListSortBenchmarking() =
    let rand = Random()

    member public this._elemCounts = [| 10000..10000..100000 |]

    [<ParamsSource("_elemCounts")>]
    member val elemCount = 0 with get, set

    member val data = [ 1 ] with get, set

    [<IterationSetup>]
    member this.initData() =
        this.data <- [ 1 .. this.elemCount ] |> List.map (fun _ -> rand.Next(-100000, 100000))


    //[<Benchmark>]
    //member this.bubbleSort() = ListSorts.bubbleSort this.data

    [<Benchmark>]
    member this.quickSort() = ListSorts.quickSort this.data

    [<Benchmark>]
    member this.mergeSort() = ListSorts.mergeSort this.data

    [<Benchmark(Baseline = true)>]
    member this.systemSort() = List.sort this.data

[<MemoryDiagnoser>]
[<SimpleJob(launchCount = 1, iterationCount = 1, warmupCount = 1)>]
type ArraySortBenchmarking() =
    let rand = Random()

    member public this._elemCounts = [| 10000..10000..20000 |]

    [<ParamsSource("_elemCounts")>]
    member val elemCount = 0 with get, set

    member val data = [| 1 |] with get, set

    [<IterationSetup>]
    member this.initData() =
        this.data <- [| 1 .. this.elemCount |] |> Array.map (fun _ -> rand.Next(-100000, 100000))

    //[<Benchmark>]
    //member this.bubbleSort() = ArraySorts.bubbleSort this.data

    [<Benchmark>]
    member this.quickSort() = ArraySorts.quickSort this.data

    [<Benchmark>]
    member this.mergeSort() = ArraySorts.mergeSort this.data

    [<Benchmark(Baseline = true)>]
    member this.systemSort() = Array.sort this.data

[<EntryPoint>]
let main args =
    //let summary = BenchmarkRunner.Run<ArraySortBenchmarking>()

    let switcher =
        BenchmarkSwitcher [| typeof<ListSortBenchmarking>; typeof<ArraySortBenchmarking> |]

    switcher.Run args |> ignore

    0

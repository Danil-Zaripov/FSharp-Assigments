module Program

open System
open Learning
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running


type ListSortBenchmarking() =
    class
        let rand = Random()
        let data = [ 1..2 ] |> List.map (fun _ -> rand.Next(-1e8 |> int, 1e8 |> int))

        [<Benchmark>]
        member this.bubbleSort() = ListSorts.bubbleSort data

        [<Benchmark>]
        member this.quickSort() = ListSorts.quickSort data

        [<Benchmark>]
        member this.mergeSort() = ListSorts.mergeSort data
    end




[<EntryPoint>]
let main _ =
    let dataSizeModifier = [ 5; 10; 20; 30; 40; 50; 100; 500; 1000; 1500 ]

    let we_want_to_do () =
        let rand = Random()

        let algorithms =
            [
              //ListSorts.bubbleSort, "bubbleSort"
              ListSorts.quickSort, "quickSort"
              ListSorts.mergeSort, "mergeSort"
              List.sort, "List.sort" ]

        let step = 1e3 |> int

        let iteration _ =
            let n = step * 75
            let lst = [ 1..n ] |> List.map (fun _ -> rand.Next(-1e8 |> int, 1e8 |> int))
            let watch = System.Diagnostics.Stopwatch.StartNew()
            let sorted = ListSorts.mergeSort lst
            watch.Stop()
            watch.ElapsedMilliseconds

        let results = [ 1..300 ] |> List.map iteration
        let avg = results |> List.map double |> List.average
        printfn $"Average: {avg}"
        let str = results |> List.map (fun x -> sprintf $"{x} ") |> Seq.reduce (+)

        let path = @"./Data.txt"
        let file = System.IO.File.WriteAllText(path, str.TrimEnd())
        ()

    let thread = Threading.Thread(we_want_to_do, 2e8 |> int32)

    thread.Start()

    while thread.IsAlive do
        Threading.Thread.Sleep(2000)

    0

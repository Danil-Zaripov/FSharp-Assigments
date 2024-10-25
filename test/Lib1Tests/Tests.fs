namespace LearningTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass() =
    member this.CheckSortingAlgorithm arr algorithm =
        let expected = arr |> Array.sort
        let actual = arr |> algorithm

        CollectionAssert.AreEqual(expected, actual)

    member this.GenerateRandomArray() =
        let rand = Random()

        [| for i in 0 .. rand.Next(10, 100) do
               rand.Next(-100, 100) |]

    [<TestMethod>]
    member this.TestBubbleSort() =
        for _ in 1..50 do
            this.CheckSortingAlgorithm (this.GenerateRandomArray()) Learning.Sorts.bubbleSort

    [<TestMethod>]
    member this.TestQuickSort() =
        for _ in 1..50 do
            this.CheckSortingAlgorithm (this.GenerateRandomArray()) Learning.Sorts.quickSort

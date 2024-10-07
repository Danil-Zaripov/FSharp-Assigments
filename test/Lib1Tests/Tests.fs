namespace LearningTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Learning


[<TestClass>]
type TestClass() =
    member this.CheckSortingAlgorithmAscending arr algorithm = 
        let expected = arr |> Array.sort
        let actual   = arr |> (Sorts.ascendingSort algorithm) 

        CollectionAssert.AreEqual(expected, actual)

    member this.CheckSortingAlgorithmDescending arr algorithm = 
        let expected = arr |> Array.sort |> Array.rev
        let actual   = arr |> (Sorts.descendingSort algorithm)
        
        CollectionAssert.AreEqual(expected, actual)

    member this.GenerateRandomArray() =
        let rand = Random()
        [| for i in 0 .. rand.Next(10, 100) do
               rand.Next(-100, 100) |]

    [<TestMethod>]
    member this.DivideFunctionTest() = 
        let tests = [ (0, 3); (0, 1); (1, 3); (1, 4) ]
        let expected = [ ((0, 1), (2, 3)); ((0, 0), (1, 1)); ((1, 1), (2, 3)); ((1, 2), (3, 4)) ]
        let actual = tests |> List.map Sorts.divide_two_halves

        Assert.AreEqual(expected, actual)

    [<TestMethod>] 
    member this.TestBubbleSortAscending() = 
        for _ in 1..50 do
            this.CheckSortingAlgorithmAscending (this.GenerateRandomArray()) Sorts.bubbleSortGeneral
    
    [<TestMethod>]
    member this.TestQuickSortAscending() = 
        for _ in 1..50 do
            this.CheckSortingAlgorithmAscending (this.GenerateRandomArray()) Sorts.quickSortGeneral

    [<TestMethod>]
    member this.TestMergeSortAscending() = 
        for _ in 1..50 do
            this.CheckSortingAlgorithmAscending (this.GenerateRandomArray()) Sorts.mergeSortGeneral

    [<TestMethod>] 
    member this.TestBubbleSortDescending() = 
        for _ in 1..50 do
            this.CheckSortingAlgorithmDescending (this.GenerateRandomArray()) Sorts.bubbleSortGeneral
    
    [<TestMethod>]
    member this.TestQuickSortDescending() = 
        for _ in 1..50 do
            this.CheckSortingAlgorithmDescending (this.GenerateRandomArray()) Sorts.quickSortGeneral

    [<TestMethod>]
    member this.TestMergeSortDescending() = 
        for _ in 1..50 do
            this.CheckSortingAlgorithmDescending (this.GenerateRandomArray()) Sorts.mergeSortGeneral
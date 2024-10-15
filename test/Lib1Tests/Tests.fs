namespace LearningTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass() =
    [<TestMethod>]
    member this.TestFactorial() =
        let expected = [ 1; 2; 6; 24; 120; 720 ]
        let actual = [ 1..6 ] |> List.map Learning.Basic.factorial

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBasicFib() =
        let expected = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89 ]
        let actual = [ 1..11 ] |> List.map Learning.Basic.fib

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestMatrixProduct() =
        let expected =
            array2D[[ 19; 22 ]
                    [ 43; 50 ]]

        let mat1 = array2D [ [ 1; 2 ]; [ 3; 4 ] ]
        let mat2 = array2D [ [ 5; 6 ]; [ 7; 8 ] ]
        let actual = Learning.Matrix.product2_2 mat1 mat2

        CollectionAssert.AreEqual(actual, expected)

    [<TestMethod>]
    member this.TestToBinary() =
        let expected =
            [ [| 1; 1; 1; 1; 1 |]; [| 1 |]; [| 1; 1 |]; [| 1; 1; 0 |]; [| 1; 0; 0; 0; 0 |] ]

        let actual =
            List.map Learning.Matrix.toBinary [ 16 + 8 + 4 + 2 + 1; 1; 1 + 2; 2 + 4; 16 ]

        (expected, actual) ||> List.zip |> List.iter CollectionAssert.AreEqual


    [<TestMethod>]
    member this.TestMatrixFibFunction() =
        for i in 1..10 do
            let expected = Learning.Basic.fib i
            let actual = Learning.Matrix.fib i

            Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestSwapFunction() =
        let arr = [| 1; 2; 3; 4; 5; 6; 10; 8; 9; 7 |]
        let expected = [| 1..10 |]
        let actual = arr
        Learning.Basic.swap &actual[6] &actual[9]

        CollectionAssert.AreEqual(expected, actual)

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
            this.CheckSortingAlgorithm (this.GenerateRandomArray()) Learning.Basic.bubbleSort

    [<TestMethod>]
    member this.TestQuickSort() =
        for _ in 1..50 do
            this.CheckSortingAlgorithm (this.GenerateRandomArray()) Learning.QuickSort.quickSort

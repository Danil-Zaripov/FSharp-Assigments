namespace LearningTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.TestFactorial() =
        let expected = 120
        let actual = Learning.Basic.factorial 5 
        
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestBubbleSort() = 
        let testArr = [|1; 4; 6; 3; 2; 100; -23; 23; -1|]

        let expected = testArr |> Array.sort
        let actual = testArr |> Learning.Basic.bubbleSort
            
                
        CollectionAssert.AreEqual(expected, actual)


    [<TestMethod>]
    member this.TestMatrixProduct() = 
        let expected = 
            array2D[
                [
                    19
                    22 
                ]
                [
                    43
                    50
                ]
            ]
        let mat1 = 
            array2D [[1;2];[3;4]]
        let mat2 = array2D [[5;6];[7;8]]
        let actual = Learning.Matrix.product2_2 mat1 mat2

        CollectionAssert.AreEqual(actual, expected)

    [<TestMethod>]
    member this.TestToBinary() = 
        let expected = [| 1; 0; 1; 1; 0; |]
        let actual   = Learning.Matrix.toBinary (16 + 0 + 4 + 2 + 0)
        CollectionAssert.AreEqual(expected, actual)


    [<TestMethod>]
    member this.TestMatrixFibFunction() = 
        for i in 1 .. 10 do

            let expected = Learning.Basic.fib i
            let actual = Learning.Matrix.fib i

            Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestSwapFunction() =
        let arr = [|1;2;3|]
        let expected = [|3;2;1|]
        let actual = arr
        Learning.Basic.swap &actual[0] &actual[2];

        
        CollectionAssert.AreEqual(expected, actual)

    member this.TestQuickSort() =
        let testArr = [|1; 4; 6; 3; 2; 100; -23; 23; -1|]

        let expected = testArr |> Array.sort

        let actual = testArr |> Learning.QuickSort.quickSort
            
                
        CollectionAssert.AreEqual(expected, actual)

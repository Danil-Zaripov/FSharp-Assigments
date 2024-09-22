namespace LearningTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.TestBubbleSort() = 
        let testArr = [|1; 4; 6; 3; 2; 100; -23; 23; -1|]

        let expected = testArr |> Array.sort

        let actual = testArr |> Learning.Say.bubbleSort
            
                
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFactorial() = 
        let actual = Learning.Say.factorial 5
        let expected = 120

        Assert.AreEqual(actual, expected)

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
        let expected = [| 1; 0; 1; |]
        let actual   = Learning.Matrix.toBinary (4 + 1)
        CollectionAssert.AreEqual(expected, actual)


    [<TestMethod>]
    member this.TestMatrixFibFunction() = 
        for i in 1 .. 10 do

            let expected = Learning.Say.fib i
            let actual = Learning.Matrix.fib i

            Assert.AreEqual(expected, actual)
        
    [<TestMethod>]
    member this.TestFunctionalBubbleSort() = 
        let testArr = [|1; 4; 6; 3; 2; 100; -23; 23; -1|]

        let expected = testArr |> Array.sort

        let actual = testArr |> Learning.Say.bubbleSort1

        CollectionAssert.AreEqual(expected, actual)


    [<TestMethod>]
    member this.TestSwapFunction() =
        let arr = [|1;2;3|]
        let expected = [|3;2;1|]
        let actual = arr
        Learning.QuickSort.swap &actual[0] &actual[2];
        
        CollectionAssert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestGetMedian() = 
        let data = [(0, 6); (0, 1); (0, 100); (3, 4); (3, 3)]
        let expected = [3; 1; 50; 4; 3]
        let actual = data |> List.map (fun (a,b) -> Learning.QuickSort.getMedian a b)

        Assert.AreEqual(expected, actual)
        
    [<TestMethod>]
    member this.TestQuickSort() =
        let testArr = [|1; 4; 6; 3; 2; 100; -23; 23; -1|]

        let expected = testArr |> Array.sort

        let actual = testArr |> Learning.QuickSort.quickSort
            
                
        CollectionAssert.AreEqual(expected, actual)

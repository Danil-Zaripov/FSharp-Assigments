namespace Lib1Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass () =
    
    [<TestMethod>]
    member this.TestBubbleSort() = 
        let testArr = [|1; 4; 6; 3; 2; 100; -23; 23; -1|]

        let expected = testArr |> Array.sort

        let actual = testArr |> Lib1.Say.bubbleSort

        let arrsAreEqual (arr1: 'a array) (arr2: 'a array) =
            if arr1.Length = arr2.Length then
                Array.forall2 (=) arr1 arr2
            else false
            
                
        Assert.IsTrue(arrsAreEqual expected actual)
        // Assert.AreEqual(expected actual) // FAILS

    [<TestMethod>]
    member this.TestFactorial() = 
        let actual = Lib1.Say.factorial 5
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
        let actual = Lib1.Matrix.product2_2 mat1 mat2

        let matsAreEqual (mat1: 'a array2d) (mat2: 'a array2d) =
            // https://forums.fsharp.org/t/comparing-2-array2ds/2240/2
            let arr1 = seq { for x in mat1 do unbox<int> x }
            let arr2 = seq { for x in mat2 do unbox<int> x }
            Seq.forall2 (=) arr1 arr2

        Assert.IsTrue(matsAreEqual actual expected)

    [<TestMethod>]
    member this.TestToBinary() = 
        let expected = [| 1; 0; 1; |]
        let actual   = Lib1.Matrix.toBinary (4 + 1)
        let arrsAreEqual (arr1: 'a array) (arr2: 'a array) =
            if arr1.Length = arr2.Length then
                Array.forall2 (=) arr1 arr2
            else false
        
        Assert.IsTrue(arrsAreEqual expected actual)


    [<TestMethod>]
    member this.TestMatrixFibFunction() = 
        for i in 1 .. 10 do

            let expected = Lib1.Say.fib i
            let actual = Lib1.Matrix.fib i

            Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFunctionalBubbleSort() = 
        let testArr = [|1; 4; 6; 3; 2; 100; -23; 23; -1|]

        let expected = testArr |> Array.sort

        let actual = testArr |> Lib1.Say.bubbleSort1

        let arrsAreEqual (arr1: 'a array) (arr2: 'a array) =
            if arr1.Length = arr2.Length then
                Array.forall2 (=) arr1 arr2
            else false
            
                
        Assert.IsTrue(arrsAreEqual expected actual)
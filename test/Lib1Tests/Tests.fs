namespace LearningTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass () =

    [<TestMethod>] 
    member this.TestBubbleSort() = 
        let rand = Random()
        let array = 
            [|
                for i in 0 .. rand.Next(10, 100) do 
                    rand.Next(-100, 100)
            |]
        let expected = array |> Array.sort
        let actual   = array |> Learning.Sorts.bubbleSort
        
        CollectionAssert.AreEqual(expected, actual)
namespace LearningTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestHelloWorld() = 
        let expected = "Hello, world!"
        let actual = Learning.Say.hello_world

        Assert.AreEqual(expected, actual)
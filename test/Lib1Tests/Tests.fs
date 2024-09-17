namespace Lib1Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestHelloWorld() = 
        let expected = "Hello, world!"
        let actual = Lib1.Say.hello_world

        Assert.AreEqual(expected, actual)
namespace Tests

open Xunit

module UnitTests =
    let checkListSort f =
        let data = [ [ 1; 2; 3; 4; 5 ]; [ 5; 4; 3; 2; 1 ]; [ 1; 1; 1; 1; 1 ]; []; [ 1 ] ]

        let expected = List.map List.sort data
        let actual = List.map f data

        List.reduce (&&) (List.map2 (=) expected actual)

    let checkArraySort f =
        let data =
            [ [| 1; 2; 3; 4; 5 |]; [| 5; 4; 3; 2; 1 |]; [| 1; 1; 1; 1; 1 |]; [||]; [| 1 |] ]

        let expected = List.map Array.sort data
        let actual = List.map f data

        List.reduce (&&) (List.map2 (fun xs ys -> System.Linq.Enumerable.SequenceEqual(xs, ys)) expected actual)

    [<Fact>]
    let UnitCheckListInsertionSort () =
        Assert.True(checkListSort Learning.ListSorts.insertionSort)

    [<Fact>]
    let UnitCheckArrayInsertionSort () =
        Assert.True(checkArraySort Learning.ArraySorts.insertionSort)

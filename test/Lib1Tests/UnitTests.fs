namespace Tests

open Xunit

module UnitTests =
    let testCases = [ []; [ 1; 2; 3; 4; 5 ]; [ 5; 4; 3; 2; 1 ] ]

    let checkForTestCases alg =
        let algorithmWorks xs = (List.sort xs) = (alg xs)

        testCases |> List.map algorithmWorks |> List.reduce (&&)

    [<Fact>]
    let ``bubbleSort works for delicate cases`` () =
        Assert.True(checkForTestCases Learning.ListSorts.bubbleSort)

    [<Fact>]
    let ``quickSort works for delicate cases`` () =
        Assert.True(checkForTestCases Learning.ListSorts.quickSort)

    [<Fact>]
    let ``mergeSort works for delicate cases`` () =
        Assert.True(checkForTestCases Learning.ListSorts.mergeSort)

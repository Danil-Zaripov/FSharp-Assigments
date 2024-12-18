namespace Tests

open Xunit
open Trees
open FSharpPlus.Data


module UnitTests =
    let tree =
        let nd_type_4_0 = MyTree.ofList ([ 5; 6; 7; 10 ] |> List.map MyTree.toLeaf)

        let nd_type_3_0 = MyTree.ofList ([ 2; 3 ] |> List.map MyTree.toLeaf)
        let nd_type_3_1 = MyTree.ofList [ nd_type_4_0 ]

        let nd_type_2_0 = MyTree.ofList [ nd_type_3_0; Leaf 4 ]
        let nd_type_2_1 = MyTree.ofList [ nd_type_3_1 ]

        let nd_type_1_0 = MyTree.ofList [ nd_type_2_0; nd_type_2_1; Leaf 1 ]

        nd_type_1_0

    let treeHeight = 5
    let treeSum = 5 + 6 + 7 + 10 + 2 + 3 + 4 + 1
    let treeProduct = 5 * 6 * 7 * 10 * 2 * 3 * 4 * 1

    [<Fact>]
    let checkSum () =
        let expected = treeSum
        let actual = MyTree.sum tree

        Assert.Equal(expected, actual)

    [<Fact>]
    let checkFoldProduct () =
        let expected = treeProduct
        let actual = MyTree.fold (*) 1 tree

        Assert.Equal(expected, actual)

    [<Fact>]
    let checkMap () =
        let expected = treeSum * 2
        let actual = tree |> MyTree.map ((*) 2) |> MyTree.fold (+) 0
        Assert.Equal(expected, actual)

    [<Fact>]
    let checkHeight () =
        let expected = treeHeight
        let actual = MyTree.height tree
        Assert.Equal(expected, actual)

    [<Fact>]
    let checkLeafNumberFunction () =
        let expected = 8
        let actual = MyTree.leafNumber tree

        Assert.Equal(expected, actual)

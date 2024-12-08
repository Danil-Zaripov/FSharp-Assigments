namespace Tests

open Xunit
open Trees

module UnitTests =
    let tree =
        Node(
            [ Node([ Leaf 3; Node [ Leaf 5; Leaf 7; Leaf 20 ]; Leaf 1 ])
              Node([ Leaf 3; Leaf 5; Node([ Leaf 8 ]) ]) ]
        )

    [<Fact>]
    let checkSumFold () =
        let expected = 3 + 5 + 7 + 20 + 1 + 3 + 5 + 8
        let actual = MyTree.fold (+) 0 tree

        Assert.Equal(expected, actual)

    [<Fact>]
    let checkMap () =
        let expected = (3 + 5 + 7 + 20 + 1 + 3 + 5 + 8) * 2
        let actual = tree |> MyTree.map ((*) 2) |> MyTree.fold (+) 0
        Assert.Equal(expected, actual)

    [<Fact>]
    let checkHeight () =
        let expected = 4
        let actual = MyTree.height tree
        Assert.Equal(expected, actual)

namespace Tests

open FsCheck
open FsCheck.Xunit
open Trees


[<Properties(MaxTest = 100)>]
module PropertyTests =
    let generateNotEmpty (xs: 'a list) =
        match xs with
        | [] -> [ Unchecked.defaultof<'a> ]
        // 0 for numeric types
        // false for bool
        // '\0' for char
        // null(!) for refs
        | _ -> xs

    let notEmptyNode = MyTree.nodeFromList << generateNotEmpty

    let genTree nd1 nd2 nd3 nd4 nd5 =
        Node(
            [ Node([ nd1 |> notEmptyNode; Node([ nd2 |> notEmptyNode ]) ])
              (Node([ Node([ nd3 |> notEmptyNode ]) ]))
              (nd4 |> notEmptyNode)
              nd5 |> notEmptyNode ]
        )

    [<Property>]
    let checkSumFold (nd1: int list) (nd2: int list) (nd3: int list) (nd4: int list) (nd5: int list) =
        let sum = List.sum

        let expected =
            (nd1 |> sum) + (nd2 |> sum) + (nd3 |> sum) + (nd4 |> sum) + (nd5 |> sum)

        let tree = genTree nd1 nd2 nd3 nd4 nd5
        let actual = MyTree.fold (+) 0 tree

        expected = actual

    [<Property>]
    let sumFoldIsEqualToFoldBack (nd1: int list) (nd2: int list) (nd3: int list) (nd4: int list) (nd5: int list) =
        let tree = genTree nd1 nd2 nd3 nd4 nd5
        let expected = MyTree.fold (+) 0 tree
        let actual = MyTree.foldBack (+) tree 0
        actual = expected

    [<Property>]
    let checkMap (nd1: int list) (nd2: int list) (nd3: int list) (nd4: int list) (nd5: int list) =
        let sum = List.sum

        let expected =
            ((nd1 |> sum) + (nd2 |> sum) + (nd3 |> sum) + (nd4 |> sum) + (nd5 |> sum)) * 5

        let tree = genTree nd1 nd2 nd3 nd4 nd5 |> MyTree.map ((*) 5)
        let actual = MyTree.fold (+) 0 tree

        expected = actual

    [<Property>]
    let checkHeight (nd1: int list) (nd2: int list) (nd3: int list) (nd4: int list) (nd5: int list) =
        let expected = 5
        let tree = genTree nd1 nd2 nd3 nd4 nd5
        let actual = MyTree.height tree

        expected = actual

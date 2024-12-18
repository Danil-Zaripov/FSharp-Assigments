namespace Tests

open FsCheck
open FsCheck.Xunit
open Trees


[<Properties(MaxTest = 100)>]
module PropertyTests =

    [<Property>]
    let checkMap (tree: int MyTree) =
        let expected = (tree |> MyTree.sum) * 5

        let actual = tree |> MyTree.map ((*) 5) |> MyTree.sum

        expected = actual

    [<Property>]
    let checkIdentityMap (tree: int MyTree) =
        let expected = tree
        let actual = tree |> MyTree.map (fun x -> x)

        expected = actual

    [<Property>]
    let foldBackEqualToFoldFrontForAddition (tree: int MyTree) =
        let foldedBackwards = 0 |> MyTree.foldBack (+) tree
        let foldedForwards = tree |> MyTree.fold (+) 0

        foldedBackwards = foldedForwards

    [<Property>]
    let mapDoesNotChangeHeight (tree: int MyTree) (f: int -> int) =
        let expected = tree |> MyTree.height
        let actual = tree |> MyTree.map f |> MyTree.height

        expected = actual

    [<Property>]
    let leafNumberDoesNotChange (tree: int MyTree) (f: int -> int) =
        let expected = tree |> MyTree.leafNumber
        let actual = tree |> MyTree.map f |> MyTree.leafNumber

        expected = actual

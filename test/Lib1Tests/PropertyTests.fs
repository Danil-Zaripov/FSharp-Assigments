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

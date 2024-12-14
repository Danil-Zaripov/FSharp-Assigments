namespace Tests

open FsCheck
open FsCheck.Xunit
open Trees


[<Properties(MaxTest = 100)>]
module PropertyTests =
    let (.=.) left right =
        left = right |> Prop.label (sprintf "%A = %A" left right)

    [<Property>]
    let matrixConversionIsCorrect (mat: int array2d) =
        match QuadTree.tryOfMatrix mat with
        | Some(actual) ->
            let expected = mat
            let actual = mat |> QuadTree.ofMatrix |> QuadTree.toMatrix

            expected .=. actual
        | None -> true .=. true

namespace Tests

open FsCheck
open FsCheck.Xunit

[<Properties(MaxTest = 100)>]
module SortTests =

    [<Property>]
    let CheckBubbleSortAlgorithm (lst: int list) =
        (lst |> Learning.ListSorts.bubbleSort) = (lst |> List.sort)

    [<Property>]
    let CheckQuickSortAlgorithm (lst: int list) =
        (lst |> Learning.ListSorts.quickSort) = (lst |> List.sort)

    [<Property>]
    let CheckMergeSortAlgorithm (lst: int list) =
        (lst |> Learning.ListSorts.mergeSort) = (lst |> List.sort)

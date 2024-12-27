namespace Tests

open FsCheck
open FsCheck.Xunit

[<Properties(MaxTest = 100)>]
module SortTests =
    [<Property>]
    let CheckListInsertionSort (xs: int list) =
        (List.sort xs) = (Learning.ListSorts.insertionSort xs)

    [<Property>]
    let CheckArrayInsertionSort (arr: int array) =
        System.Linq.Enumerable.SequenceEqual(Array.sort arr, Learning.ArraySorts.insertionSort arr)

namespace Tests

open FsCheck
open FsCheck.Xunit

[<Properties(MaxTest = 100)>]
module SortTests =
    let placeholder() = ()
namespace Tests

open FsCheck
open FsCheck.Xunit

[<Properties(MaxTest = 100)>]
module PropertyTests =
    let placeholder () = ()

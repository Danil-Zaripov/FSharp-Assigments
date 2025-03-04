namespace Tests

open FsCheck
open FsCheck.Xunit
open ImageProcessing


[<Properties(MaxTest = 100)>]
module PropertyTests =
    let placeholder () = ()

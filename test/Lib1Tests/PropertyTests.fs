namespace Tests

open FsCheck
open FsCheck.Xunit
open ImageProcessing


[<Properties(MaxTest = 100)>]
module PropertyTests =

    [<Property>]
    let checkIdentityFilter (data: byte[,]) =
        let tst () =
            let expected = Array2D.copy data
            let actual = applyFilter idKernel data

            expected = actual

        tst ()

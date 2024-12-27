namespace Tests

open FsCheck
open FsCheck.Xunit
open ImageProcessing


[<Properties(MaxTest = 100)>]
module PropertyTests =

    [<Property>]
    let checkIdentityFilter (data: byte[,]) =
        let expected = Array2D.copy data
        let actual = applyFilter idKernel data

        expected = actual

    [<Property>]
    let checkBlackFilter (data: byte[,]) =
        let expected = Array2D.zeroCreate (Array2D.length1 data) (Array2D.length2 data)
        let actual = applyFilter blackKernel data

        expected = actual

    [<Property>]
    let checkWhiteFilter (data: byte[,]) =
        let expected = data |> Array2D.map (fun x -> if x > 0uy then 255uy else 0uy)
        let actual = applyFilter whiteKernel data

        expected = actual

    [<Property>]
    let sizeDoesntChange (data: byte[,]) (f: float32 -> float32) =
        let filter =
            let rand = System.Random()
            let randomize _ = rand.Next(-100, 100)
            gaussianBlurKernel |> Array.map (Array.map (randomize >> float32 >> f))

        let expected = Array2D.length1 data, Array2D.length2 data

        let actual =
            let filtered = data |> applyFilter filter
            Array2D.length1 filtered, Array2D.length2 filtered

        expected = actual

    [<Property>]
    let shiftingIsAssociative (data: byte[,]) =
        let actual = data |> applyFilter shiftRightDownKenrel
        let expected = data |> applyFilter shiftDownKernel |> applyFilter shiftRightKernel
        let n, m = Array2D.length1 data, Array2D.length2 data
        (expected[1 .. n - 2, 1 .. m - 2] = actual[1 .. n - 2, 1 .. m - 2])

    [<Property>]
    let shiftingIsKommutative (data: byte[,]) =
        let downRight = data |> applyFilter shiftDownKernel |> applyFilter shiftRightKernel
        let rightDown = data |> applyFilter shiftRightKernel |> applyFilter shiftDownKernel

        downRight = rightDown

    [<Property>]
    let expandedFormIsTheSame (data: byte[,]) (f: float32 -> float32) =
        let filter =
            let rand = System.Random()
            let randomize _ = rand.Next(-100, 100)
            gaussianBlurKernel |> Array.map (Array.map (randomize >> float32 >> f))

        let expanded = expand filter
        let expected = data |> applyFilter filter
        let actual = data |> applyFilter expanded

        expected = actual

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
        let n, m = Array2D.getDims mat

        let tst () =
            let expected = mat
            let actual = mat |> QuadTree.ofMatrix |> QuadTree.toMatrix

            expected = actual

        (n <> 0 && m <> 0) ==> (lazy tst ())

    [<Property>]
    let mapIsCorrect (mat: int array2d) =
        match QuadTree.tryOfMatrix mat with
        | Some(actual) ->
            let expected = (mat |> Array2D.fold (+) 0) * 2

            let actual =
                mat
                |> QuadTree.ofMatrix
                |> QuadTree.map ((*) 2)
                |> QuadTree.toMatrix
                |> Array2D.fold (+) 0

            expected .=. actual
        | None -> true .=. true

    [<Property>]
    let getElementIsCorrect (mat: int array2d) =
        match QuadTree.tryOfMatrix mat with
        | Some(actual) ->
            let mutable isOk = true
            let n, m = Array2D.length1 mat, Array2D.length2 mat

            for i in 0 .. n - 1 do
                for j in 0 .. m - 1 do
                    if (mat[i, j] <> QuadTree.getElement (i, j) actual) then
                        isOk <- false

            isOk
        | None -> true

    let getRandomArray2D n m =
        let rand = System.Random()
        let mat = Array2D.map (fun _ -> rand.Next(-1000, 1000)) (Array2D.zeroCreate n m)
        mat


    [<Property>]
    let map2SumIsCorrect (mat1: int array2d) =
        let n, m = Array2D.getDims mat1

        let tst () =
            let mat2 = getRandomArray2D n m
            let tr1 = mat1 |> QuadTree.ofMatrix
            let tr2 = mat2 |> QuadTree.ofMatrix

            let actual = (QuadTree.map2 (+) tr1 tr2) |> QuadTree.toMatrix

            let expected = Array2D.copy mat1 |> Array2D.mapi (fun i j x -> x + mat2[i, j])

            expected .=. actual

        (n <> 0 && m <> 0) ==> (lazy tst ())

    [<Property>]
    let multiplyIsCorrect (mat1: int array2d) =
        let n, m = Array2D.getDims mat1

        let tst () =
            let d = (System.Random().Next(1, 500))
            let mat2 = getRandomArray2D m d
            let tr1 = mat1 |> QuadTree.ofMatrix
            let tr2 = mat2 |> QuadTree.ofMatrix

            let actual = QuadTree.multiply (*) (+) 0 tr1 tr2

            let expected =
                let mat = Array2D.zeroCreate n d

                for i in 0 .. n - 1 do
                    for j in 0 .. d - 1 do
                        for k in 0 .. m - 1 do
                            mat[i, j] <- mat[i, j] + mat1[i, k] * mat2[k, j]

                mat

            actual |> QuadTree.toMatrix .=. expected

        (n <> 0 && m <> 0) ==> (lazy tst ())

    [<Property>]
    let multiplyBigintIsCorrect (mat1: bigint array2d) =
        let n, m = Array2D.getDims mat1

        let tst () =
            let d = (System.Random().Next(1, 500))
            let mat2 = getRandomArray2D m d |> Array2D.map bigint
            let tr1 = mat1 |> QuadTree.ofMatrix
            let tr2 = mat2 |> QuadTree.ofMatrix

            let actual = QuadTree.multiply (*) (+) (0 |> bigint) tr1 tr2

            let expected =
                let mat = Array2D.zeroCreate n d

                for i in 0 .. n - 1 do
                    for j in 0 .. d - 1 do
                        for k in 0 .. m - 1 do
                            mat[i, j] <- mat[i, j] + mat1[i, k] * mat2[k, j]

                mat

            actual |> QuadTree.toMatrix .=. expected

        (n <> 0 && m <> 0) ==> (lazy tst ())

namespace Tests

open Xunit
open Trees


module UnitTests =
    let n, m = 4, 4
    let zeroMatrix: int array2d = Array2D.zeroCreate n m

    [<Fact>]
    let checkSmallMatrix () =
        let mat: int array2d = Array2D.zeroCreate 1 2
        mat[0, 1] <- 1
        let expected = mat
        printfn "%A" (mat |> QuadTree.ofMatrix)
        let actual = mat |> QuadTree.ofMatrix |> QuadTree.toMatrix
        Assert.Equal(expected, actual)

    [<Fact>]
    let check1ElementMatrix () =
        let mat: int array2d = Array2D.zeroCreate 1 1
        let expected = mat
        let actual = mat |> QuadTree.ofMatrix |> QuadTree.toMatrix
        Assert.Equal(expected, actual)

    [<Fact>]
    let checkZeroMatrixQuadTree () =
        let expected = zeroMatrix
        let actual = zeroMatrix |> QuadTree.ofMatrix |> QuadTree.toMatrix

        Assert.Equal(expected, actual)

    [<Fact>]
    let nonEvenSquareMatrixCheck () =
        let mat: int array2d = Array2D.zeroCreate 3 3
        mat[0, 0] <- 1
        mat[2, 2] <- 1
        mat[0, 2] <- 1
        mat[2, 0] <- 1

        let expected = mat
        let actual = mat |> QuadTree.ofMatrix |> QuadTree.toMatrix
        Assert.Equal(expected, actual)
//    let nonZeroMatrix =
//        let zeroMatrix: int array2d = Array2D.zeroCreate n m

//        zeroMatrix[0, 3] <- 1
//        zeroMatrix


//    [<Fact>]
//    let checkNonZeroMatrix () =
//        let qTree = QuadTree.ofMatrix nonZeroMatrix
//        let actual = qTree

//        let zeroes =
//            { NW = Leaf 0
//              NE = Leaf 0
//              SW = Leaf 0
//              SE = Leaf 0 }

//        let expected =
//            { bounds = { n = 4; m = 4 }
//              tree =
//                Node(
//                    { zeroes with
//                        NE = (Node({ zeroes with NE = Leaf 1 })) }
//                ) }

//        Assert.Equal(actual, expected)

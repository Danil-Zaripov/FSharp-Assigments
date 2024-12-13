namespace Tests

open Xunit
open Trees


module UnitTests =
    let n, m = 4, 4
    let zeroMatrix: int array2d = Array2D.zeroCreate n m

    let nonZeroMatrix =
        let zeroMatrix: int array2d = Array2D.zeroCreate n m

        zeroMatrix[0, 3] <- 1
        zeroMatrix



    [<Fact>]
    let checkZeroMatrixQuadTree () =
        let qTree = QuadTree.ofMatrix zeroMatrix
        let actual = qTree

        let expected =
            { bounds = { n = 4; m = 4 }
              tree = Leaf 0 }

        Assert.Equal(actual, expected)

    [<Fact>]
    let checkNonZeroMatrix () =
        let qTree = QuadTree.ofMatrix nonZeroMatrix
        let actual = qTree

        let zeroes =
            { NW = Leaf 0
              NE = Leaf 0
              SW = Leaf 0
              SE = Leaf 0 }

        let expected =
            { bounds = { n = 4; m = 4 }
              tree =
                Node(
                    { zeroes with
                        NE = (Node({ zeroes with NE = Leaf 1 })) }
                ) }

        Assert.Equal(actual, expected)

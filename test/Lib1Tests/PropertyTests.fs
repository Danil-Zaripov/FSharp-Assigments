namespace Tests

open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp
open Trees
open Graphs

module PropertyTests =

    type AdjacencyMatrix =
        static member AdjacencyMatrix() =
            let adjacencyMatrixGenerator =
                gen {
                    let! n = Gen.choose (1, 50)

                    let getMatrix n =
                        gen {
                            let mat = Array2D.init n n (fun x y -> x = y)
                            let elems = n * n
                            let! nonBaseElems = Gen.choose (0, elems / 100 + elems % 100)

                            for _ in 1..nonBaseElems do
                                let! x = Gen.choose (0, n - 1)
                                let! y = Gen.choose (0, n - 1)
                                mat[x, y] <- true

                            return mat
                        }

                    let! mat = getMatrix n
                    return mat
                }

            Arb.fromGen (adjacencyMatrixGenerator)

    type AdjacencyMatrixWeighted =
        static member AdjacencyMatrix() =
            let adjacencyMatrixGenerator =
                gen {
                    let! n = Gen.choose (1, 50)

                    let getMatrix n =
                        gen {
                            let mat = Array2D.create n n None
                            let elems = n * n
                            let! nonBaseElems = Gen.choose (0, elems / 100 + elems % 100)

                            for _ in 1..nonBaseElems do
                                let! x = Gen.choose (0, n - 1)
                                let! y = Gen.choose (0, n - 1)
                                let! r = Gen.choose (1, 1000)
                                mat[x, y] <- Some r

                            return (Array2D.mapi (fun x y c -> if x = y then Some 0 else c) mat)
                        }

                    let! mat = getMatrix n
                    return mat
                }

            Arb.fromGen (adjacencyMatrixGenerator)

    type AdjacencyMatrixWeightedBigint =
        static member AdjacencyMatrix() =
            let adjacencyMatrixGenerator =
                gen {
                    let! n = Gen.choose (1, 50)

                    let getMatrix n =
                        gen {
                            let mat = Array2D.create n n None
                            let elems = n * n
                            let! nonBaseElems = Gen.choose (0, elems / 100 + elems % 100)

                            for _ in 1..nonBaseElems do
                                let mutable VERYBIGINT = bigint -1
                                let _ = bigint.TryParse("12345678912345678917", &VERYBIGINT)
                                let! x = Gen.choose (0, n - 1)
                                let! y = Gen.choose (0, n - 1)
                                let! r = Gen.choose (1, 2000000)

                                mat[x, y] <- Some(r |> bigint |> (*) VERYBIGINT)

                            return (Array2D.mapi (fun x y c -> if x = y then Some(bigint 0) else c) mat)
                        }

                    let! mat = getMatrix n
                    return mat
                }

            Arb.fromGen (adjacencyMatrixGenerator)

    [<Property(Arbitrary = [| typeof<AdjacencyMatrix> |])>]
    let checkTransitiveClosure mat =
        let g = QuadTree.ofMatrix mat
        let actual = Graph.transitiveClosure g |> QuadTree.toMatrix
        let expected = Utility.warshall mat
        expected = actual

    [<Property(Arbitrary = [| typeof<AdjacencyMatrix> |])>]
    let checkShortestPath mat =
        let g = QuadTree.ofMatrix mat
        let actual = g |> Graph.shortestPath |> QuadTree.toMatrix
        let expected = Utility.warshallPath mat
        expected = actual

    [<Property(Arbitrary = [| typeof<AdjacencyMatrixWeighted> |])>]
    let checkShortestPathWeighted mat =
        let g = QuadTree.ofMatrix mat
        let actual = g |> WeightedGraph.shortestPath (+) |> QuadTree.toMatrix
        let expected = mat |> Utility.warshallPathWeighted (+)
        actual = expected

    [<Property(Arbitrary = [| typeof<AdjacencyMatrixWeightedBigint> |])>]
    let checkShortestPathWeightedBigint (mat: bigint option array2d) =
        let g = QuadTree.ofMatrix mat
        let actual = g |> WeightedGraph.shortestPath (+) |> QuadTree.toMatrix
        let expected = mat |> Utility.warshallPathWeighted (+)
        actual = expected

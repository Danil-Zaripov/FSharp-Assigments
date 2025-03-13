namespace Graphs

open Trees

type Graph = bool QuadTree

type 'T WeightedGraph = 'T Option QuadTree


module Utility =

    module Option =
        let min x y =
            match x with
            | Some xval ->
                match y with
                | Some yval -> Some(min xval yval)
                | None -> x
            | None -> y

    let warshall mat =
        let mat = Array2D.copy mat
        let V = Array2D.length1 mat

        for k in 0 .. V - 1 do
            for i in 0 .. V - 1 do
                for j in 0 .. V - 1 do
                    if (mat[i, k] && mat[k, j]) then
                        mat[i, j] <- true

        mat

    let warshallPathWeighted opAdd mat =
        let V = Array2D.length1 mat

        for k in 0 .. V - 1 do
            for i in 0 .. V - 1 do
                for j in 0 .. V - 1 do
                    mat[i, j] <- Option.min mat[i, j] (Option.map2 opAdd mat[i, k] mat[k, j])

        mat

    let warshallPath mat =
        let mat =
            Array2D.mapi
                (fun x y c ->
                    if x = y then Some 0
                    else if c then Some 1
                    else None)
                mat

        warshallPathWeighted (+) mat

    let unitMatrix n =
        Array2D.init n n (fun x y -> if x = y then 1 else 0)


open Utility

module WeightedGraph =

    let shortestPath opAdd (g: 'a WeightedGraph) =
        let V = Utility.getSegmentLength g.bounds.row

        let mutable cnt = 1
        let mutable g = g

        while cnt < V do
            g <- QuadTree.trim (QuadTree.multiply (Option.map2 opAdd) (Option.min) None g g)
            cnt <- cnt * 2

        g

module Graph =

    let transitiveClosure (g: Graph) =
        let V = Utility.getSegmentLength g.bounds.row

        let mutable cnt = 1
        let mutable g = g

        while cnt < V do
            g <- QuadTree.trim (QuadTree.multiplyConfigurable (fun x y -> y) (&&) (||) false g g)
            cnt <- cnt * 2

        g

    let shortestPath (g: Graph) : int WeightedGraph =
        let V = Utility.getSegmentLength g.bounds.row

        let unitQTree =
            unitMatrix V |> QuadTree.ofMatrix |> QuadTree.map ((*) -1) |> QuadTree.map Some

        g
        |> QuadTree.map (fun x -> if x then Some 1 else None)
        |> QuadTree.map2 (Option.map2 (+)) unitQTree // sets zero on the main diagonal
        |> WeightedGraph.shortestPath (+)

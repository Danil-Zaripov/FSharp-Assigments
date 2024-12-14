module Trees

type Bounds = { row: int * int; col: int * int } // boundary: starting .. ending

type SubNodes<'a> =
    { NW: Option<QuadTree<'a>> // NW is always Some
      NE: Option<QuadTree<'a>>
      SW: Option<QuadTree<'a>>
      SE: Option<QuadTree<'a>> }

    member this.asSeq =
        seq {
            this.NW
            this.NE
            this.SW
            this.SE
        }

and _QuadTree<'a> =
    | Node of SubNodes<'a>
    | Leaf of 'a


and QuadTree<'a> = { bounds: Bounds; tree: _QuadTree<'a> }




module Array2D =
    let getDims mat =
        Array2D.length1 mat, Array2D.length2 mat

    let fold f st mat =
        let n, m = getDims mat
        let mutable st = st

        for i in 0 .. n - 1 do
            for j in 0 .. m - 1 do
                st <- f st mat[i, j]

        st

    let reduce f mat =
        let n, m = getDims mat
        let mutable st = mat[0, 0]

        for j in 1 .. m - 1 do
            st <- f st mat[0, j]

        for i in 1 .. n - 1 do
            for j in 0..1 .. m - 1 do
                st <- f st mat[i, j]

        st

module QuadTree =
    let getSegmentLength (st, en) = en - st + 1

    let inSegment x (st, en) = st <= x && x <= en

    let addConst alpha (x, y) = (x + alpha, y + alpha)

    let halve (left, right) =
        let length = getSegmentLength (left, right) // end-inclusive

        if length > 1 then
            let half_length = length / 2

            (left, left + half_length - 1), Some(left + half_length, right)
        else
            (left, right), None

    let ofMatrix mat =
        let rec _f mat xOff yOff =
            let n, m = Array2D.length1 mat, Array2D.length2 mat
            let first = mat[0, 0]
            let isWhole = mat |> Array2D.map ((=) first) |> Array2D.reduce (&&)

            if isWhole then
                _QuadTree.Leaf first
            else

                let (top, bot) = halve (0, (n - 1))
                let (left, right) = halve (0, (m - 1))

                let getSub ns ms =
                    let i1, i2 = ns
                    let j1, j2 = ms

                    Some(
                        { bounds =
                            { row = ns |> addConst xOff
                              col = ms |> addConst yOff }
                          tree = _f mat[i1..i2, j1..j2] (xOff + i1) (yOff + j1) }
                    )

                let NW = getSub top left

                let NE =
                    match right with
                    | Some(right) -> getSub top right
                    | None -> None

                let SW =
                    match bot with
                    | Some(bot) -> getSub bot left
                    | None -> None

                let SE =
                    match bot, right with
                    | Some(bot), Some(right) -> getSub bot right
                    | _ -> None

                let subs = { NW = NW; NE = NE; SW = SW; SE = SE }

                Node(subs)

        let n, m = Array2D.getDims mat

        { bounds = { row = (0, n - 1); col = (0, m - 1) }
          tree = _f mat 0 0 }

    let tryOfMatrix mat =
        let n, m = Array2D.length1 mat, Array2D.length2 mat
        if n = 0 || m = 0 then None else Some(ofMatrix mat)

    let toMatrix (root: 'a QuadTree) =
        let n, m = root.bounds.row |> getSegmentLength, root.bounds.col |> getSegmentLength

        let mat: 'a array2d = Array2D.zeroCreate n m

        let rec _f tr =
            match tr.tree with
            | Leaf x ->
                let nSt, nEn = tr.bounds.row
                let mSt, mEn = tr.bounds.col

                for i in nSt..nEn do
                    for j in mSt..mEn do
                        mat[i, j] <- x
            | Node(subs) -> Seq.iter (Option.iter _f) subs.asSeq

        _f root

        mat

    //let rec map f tr =
    //    match tr.tree with
    //    | Node({ NW = NW; NE = NE; SW = SW; SE = SE }) ->
    //        Node(
    //            { NW = { bounds = NW.bounds; tree = map f NW }
    //              NE = { bounds = NE.bounds; tree = map f NE }
    //              SW = { bounds = SW.bounds; tree = map f SW }
    //              SE = { bounds = SE.bounds; tree = map f SE } }
    //        )
    //    | Leaf x -> Leaf(f x)

    let getElement x y root = ()

//let map2 f tr1 tr2 =
//    let dims =
//        let n1 = getSegmentLength tr1.bound.rows
//        let n2 = getSegmentLength tr2.bound.rows

//        let m1 = getSegmentLength tr1.bound.cols
//        let m2 = getSegmentLength tr2.bound.cols

//        max n1 n2, max m1 m2

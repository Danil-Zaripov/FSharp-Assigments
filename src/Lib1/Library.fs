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

    let subSegm (x1, x2) (y1, y2) =
        if y1 <= x1 && x2 <= y2 then (x1, x2)
        else if x1 <= y1 && y2 <= x2 then (y1, y2)
        else failwith "Neither is a subsegment"

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

    let rec map f tr =
        match tr.tree with
        | Node({ NW = NW; NE = NE; SW = SW; SE = SE }) ->
            { bounds = tr.bounds
              tree =
                Node(
                    { NW = Option.map (map f) NW
                      NE = Option.map (map f) NE
                      SW = Option.map (map f) SW
                      SE = Option.map (map f) SE }
                ) }
        | Leaf x -> { bounds = tr.bounds; tree = Leaf(f x) }

    let inQuadTree (i, j) tr =
        let bounds = tr.bounds
        (inSegment i bounds.row) && (inSegment j bounds.col)

    let getElement (x, y) root =
        let rec _getElement x y tr =
            if not (inQuadTree (x, y) tr) then
                None
            else
                match tr.tree with
                | Leaf x -> Some x
                | Node(subs) -> subs.asSeq |> Seq.map (Option.bind (_getElement x y)) |> Seq.find Option.isSome

        match _getElement x y root with
        | None -> invalidArg "x,y" "Index out of range"
        | Some x -> x

    let map2 f tr1 tr2 =
        let lengthsAreEqual =
            let n1, n2 = getSegmentLength tr1.bounds.row, getSegmentLength tr2.bounds.row
            let m1, m2 = getSegmentLength tr1.bounds.col, getSegmentLength tr2.bounds.col
            n1 = n2 && m1 = m2

        if lengthsAreEqual then
            let rec _map2 f tr1 tr2 =
                match tr1.tree, tr2.tree with
                | Leaf x, Leaf y ->
                    { bounds =
                        { row = subSegm tr1.bounds.row tr2.bounds.row
                          col = subSegm tr1.bounds.col tr2.bounds.col }
                      tree = Leaf(f x y) }
                | Node({ NW = NW1
                         NE = NE1
                         SW = SW1
                         SE = SE1 }),
                  Node({ NW = NW2
                         NE = NE2
                         SW = SW2
                         SE = SE2 }) ->
                    { bounds = tr1.bounds
                      tree =
                        Node(
                            { NW = Option.map2 (_map2 f) NW1 NW2
                              NE = Option.map2 (_map2 f) NE1 NE2
                              SW = Option.map2 (_map2 f) SW1 SW2
                              SE = Option.map2 (_map2 f) SE1 SE2 }
                        ) }
                | Node({ NW = NW; NE = NE; SW = SW; SE = SE }), _ ->
                    { bounds = tr1.bounds
                      tree =
                        Node(
                            { NW = Option.map2 (_map2 f) NW (Some(tr2))
                              NE = Option.map2 (_map2 f) NE (Some(tr2))
                              SW = Option.map2 (_map2 f) SW (Some(tr2))
                              SE = Option.map2 (_map2 f) SE (Some(tr2)) }
                        ) }
                | _, Node({ NW = NW; NE = NE; SW = SW; SE = SE }) ->
                    { bounds = tr1.bounds
                      tree =
                        Node(
                            { NW = Option.map2 (_map2 f) (Some(tr1)) NW
                              NE = Option.map2 (_map2 f) (Some(tr1)) NE
                              SW = Option.map2 (_map2 f) (Some(tr1)) SW
                              SE = Option.map2 (_map2 f) (Some(tr1)) SE }
                        ) }

            _map2 f tr1 tr2
        else
            invalidArg "tr1 tr2" "QuadTrees represented matrices with different sizes"

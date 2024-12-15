module Trees

// warning FS0049: Uppercase variable identifiers should
// not generally be used in patterns, and may indicate a
// missing open declaration or a misspelt pattern name.
// From the QuadTree.map2 function
#nowarn "0049"

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



module Utility =
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

open Utility

module Bounds =
    let getDims bnds =
        getSegmentLength bnds.row, getSegmentLength bnds.col

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

module SubNodes =
    let map f { NW = NW; NE = NE; SW = SW; SE = SE } =
        { NW = Option.map f NW
          NE = Option.map f NE
          SW = Option.map f SW
          SE = Option.map f SE }

    let map2
        f
        { NW = NW1
          NE = NE1
          SW = SW1
          SE = SE1 }
        { NW = NW2
          NE = NE2
          SW = SW2
          SE = SE2 }
        =
        { NW = Option.map2 f NW1 NW2
          NE = Option.map2 f NE1 NE2
          SW = Option.map2 f SW1 SW2
          SE = Option.map2 f SE1 SE2 }

    let fill tr = { NW = tr; NE = tr; SW = tr; SE = tr }

module QuadTree =
    let create bounds tree = { bounds = bounds; tree = tree }

    let createNode bounds subs = { bounds = bounds; tree = Node(subs) }

    let ofMatrix mat =
        let rec _f mat xOff yOff =
            let n, m = Array2D.getDims mat
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
        let n, m = Array2D.getDims mat
        if n = 0 || m = 0 then None else Some(ofMatrix mat)

    let toMatrix (root: 'a QuadTree) =
        let n, m = Bounds.getDims root.bounds

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
        | Node(subs) -> createNode tr.bounds (SubNodes.map (map f) subs)
        | Leaf x -> create tr.bounds (Leaf(f x))

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

                | Node(subs1), Node(subs2) -> createNode tr1.bounds (SubNodes.map2 (_map2 f) subs1 subs2)
                | Node(subs1), _ -> createNode tr1.bounds (SubNodes.map2 (_map2 f) subs1 (SubNodes.fill (Some(tr2))))
                | _, Node(subs2) -> createNode tr1.bounds (SubNodes.map2 (_map2 f) (SubNodes.fill (Some(tr1))) subs2)

            _map2 f tr1 tr2
        else
            invalidArg "tr1 tr2" "QuadTrees represented matrices with different sizes"

    let multiply tr1 tr2 =
        let (n1, m1), (n2, m2) = Bounds.getDims tr1.bounds, Bounds.getDims tr2.bounds

        if m1 <> n2 then
            invalidArg "tr1 tr2" "Matrices cannot be multiplied"
        else
            // The only way I know of
            // Sadly
            let mat = Array2D.zeroCreate n1 m2

            for i in 0 .. n1 - 1 do
                for j in 0 .. m2 - 1 do
                    for k in 0 .. n2 - 1 do
                        mat[i, j] <- mat[i, j] + (getElement (i, k) tr1) * (getElement (k, j) tr2)

            mat

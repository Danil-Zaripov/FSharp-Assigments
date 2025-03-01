module Trees

// warning FS0049: Uppercase variable identifiers should
// not generally be used in patterns, and may indicate a
// missing open declaration or a misspelt pattern name.
// From the QuadTree.map2 function
#nowarn "0049"

type Bounds = { row: int * int; col: int * int } // boundary: starting .. ending

type SubNodes<'a> =
    { NW: Option<QuadTree<'a>>
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

    let isInSegment x (st, en) = st <= x && x <= en

    // First is a subsegment of the second
    let isSubSegm (x, y) (a, b) = a <= x && y <= b

    let oneIsSubSegm segm1 segm2 =
        isSubSegm segm1 segm2 || isSubSegm segm2 segm1

    let isHardSubSegm (x, y) (a, b) =
        isSubSegm (x, y) (a, b) && (x <> a || y <> b)

    let addConst alpha (x, y) = (x + alpha, y + alpha)

    let subSegm fst snd =
        if isSubSegm fst snd then fst
        else if isSubSegm snd fst then snd
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

module QuadTree =
    let create bounds tree = { bounds = bounds; tree = tree }

    let createNode bounds subs = { bounds = bounds; tree = Node(subs) }

    let divideLeaf tr =
        match tr.tree with
        | Leaf _ ->
            let (n, m) = Bounds.getDims tr.bounds

            if n > 1 || m > 1 then
                let top, bot = halve tr.bounds.row
                let left, right = halve tr.bounds.col
                let top, left = Some(top), Some(left)

                let create row col =
                    match row, col with
                    | Some(row), Some(col) ->
                        Some(
                            { tr with
                                bounds = { row = row; col = col } }
                        )
                    | _ -> None

                let subs =
                    { NW = create top left
                      NE = create top right
                      SW = create bot left
                      SE = create bot right }

                { tr with tree = Node(subs) }
            else
                tr
        | _ -> failwith "Can't divide a Node"


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
        (isInSegment i bounds.row) && (isInSegment j bounds.col)

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
                let fill tr = { NW = tr; NE = tr; SW = tr; SE = tr }

                match tr1.tree, tr2.tree with
                | Leaf x, Leaf y ->
                    { bounds =
                        { row = subSegm tr1.bounds.row tr2.bounds.row
                          col = subSegm tr1.bounds.col tr2.bounds.col }
                      tree = Leaf(f x y) }

                | Node(subs1), Node(subs2) -> createNode tr1.bounds (SubNodes.map2 (_map2 f) subs1 subs2)
                | Node(subs1), _ -> createNode tr1.bounds (SubNodes.map2 (_map2 f) subs1 (fill (Some(tr2))))
                | _, Node(subs2) -> createNode tr1.bounds (SubNodes.map2 (_map2 f) (fill (Some(tr1))) subs2)

            _map2 f tr1 tr2
        else
            invalidArg "tr1 tr2" "QuadTrees represented matrices with different sizes"

    let multiply opMult opAdd genericZero tr1 tr2 =
        let add = map2 opAdd

        let rec _mult tr1 tr2 =
            let operation
                { NW = NW1
                  NE = NE1
                  SW = SW1
                  SE = SE1 }
                { NW = NW2
                  NE = NE2
                  SW = SW2
                  SE = SE2 }
                =
                let _mult x y = Option.map2 _mult x y
                let add = Option.map2 add

                if not (Option.isNone NE1 && Option.isNone SW2) then
                    let NW = add (_mult NW1 NW2) (_mult NE1 SW2)
                    let NE = add (_mult NW1 NE2) (_mult NE1 SE2)
                    let SW = add (_mult SW1 NW2) (_mult SE1 SW2)
                    let SE = add (_mult SW1 NE2) (_mult SE1 SE2)

                    { NW = NW; NE = NE; SW = SW; SE = SE }
                else
                    let getNeutralCol (x: Option<QuadTree<'a>>) =
                        Some
                            { bounds = { x.Value.bounds with col = (-1, -1) }
                              tree = Leaf genericZero }

                    let getNeutralRow (x: Option<QuadTree<'a>>) =
                        Some
                            { bounds = { x.Value.bounds with row = (-1, -1) }
                              tree = Leaf genericZero }

                    let NE1 = getNeutralCol NW1
                    let SE1 = getNeutralCol SW1
                    let SW2 = getNeutralRow NW2
                    let SE2 = getNeutralRow NE2

                    let NW = add (_mult NW1 NW2) (_mult NE1 SW2)
                    let NE = add (_mult NW1 NE2) (_mult NE1 SE2)
                    let SW = add (_mult SW1 NW2) (_mult SE1 SW2)
                    let SE = add (_mult SW1 NE2) (_mult SE1 SE2)

                    { NW = NW; NE = NE; SW = SW; SE = SE }

            let isOneByOne tr =
                getSegmentLength tr.bounds.row = 1 && getSegmentLength tr.bounds.col = 1

            let rec constantMult c bnd byRow tr =
                match tr.tree with
                | Node subs ->
                    { bounds =
                        { row = (if byRow then bnd else tr.bounds.row)
                          col = (if byRow then tr.bounds.col else bnd) }
                      tree = Node(SubNodes.map (constantMult c bnd byRow) subs) }
                | Leaf x ->
                    { bounds =
                        { row = (if byRow then bnd else tr.bounds.row)
                          col = (if byRow then tr.bounds.col else bnd) }
                      tree = Leaf(opMult x c) }

            match tr1.tree, tr2.tree with
            | Node subs1, Node subs2 ->
                let res = operation subs1 subs2

                let tr =
                    if Option.isSome res.NE || Option.isSome res.SW then
                        Node res
                    else
                        (Option.get res.NW).tree

                { bounds =
                    { row = tr1.bounds.row
                      col = tr2.bounds.col }
                  tree = tr }
            | Node subs, Leaf x when isOneByOne tr2 -> constantMult x tr2.bounds.col false tr1
            | Leaf x, Node subs when isOneByOne tr1 -> constantMult x tr1.bounds.row true tr2
            | Node _, Leaf _ -> _mult tr1 (divideLeaf tr2)
            | Leaf _, Node _ -> _mult (divideLeaf tr1) (tr2)
            | Leaf x, Leaf y ->
                let value =
                    seq { for _ in 1 .. (getSegmentLength tr1.bounds.col) -> opMult x y }
                    |> Seq.reduce opAdd

                { bounds =
                    { row = tr1.bounds.row
                      col = tr2.bounds.col }
                  tree = Leaf value }

        _mult tr1 tr2

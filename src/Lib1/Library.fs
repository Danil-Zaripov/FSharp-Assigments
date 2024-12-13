module Trees

type SubNodes<'a> =
    { NW: _QuadTree<'a>
      NE: _QuadTree<'a>
      SW: _QuadTree<'a>
      SE: _QuadTree<'a> }

and _QuadTree<'a> =
    | Node of SubNodes<'a>
    | Leaf of 'a

and Bounds = { n: int; m: int }

type QuadTree<'a> = { bounds: Bounds; tree: _QuadTree<'a> }

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
    let divide_two_halves (left, right) =
        let length = right - left + 1 // end-inclusive
        let half_length = length / 2

        (left, left + half_length - 1), (left + half_length, right)

    let ofMatrix mat =
        let rec _f mat =
            let n, m = Array2D.length1 mat, Array2D.length2 mat
            let first = mat[0, 0]
            let isWhole = mat |> Array2D.map ((=) first) |> Array2D.reduce (&&)

            if isWhole then
                _QuadTree.Leaf first
            else
                let (top, bot) = divide_two_halves (0, (n - 1))
                let (left, right) = divide_two_halves (0, (m - 1))
                let ((t1, t2), (b1, b2)) = (top, bot)
                let ((l1, l2), (r1, r2)) = (left, right)

                let subs =
                    { NW = _f mat[t1..t2, l1..l2]
                      NE = _f mat[t1..t2, r1..r2]
                      SW = _f mat[b1..b2, l1..l2]
                      SE = _f mat[b1..b2, r1..r2] }

                Node(subs)

        let n, m = Array2D.getDims mat

        { bounds = { n = n; m = m }
          tree = _f mat }


    let rec map f =
        function
        | Node({ NW = NW; NE = NE; SW = SW; SE = SE }) ->
            Node(
                { NW = map f NW
                  NE = map f NE
                  SW = map f SW
                  SE = map f SE }
            )
        | Leaf x -> Leaf(f x)

    let rec map2 f tr1 tr2 =
        match tr1, tr2 with
        | Node({ NW = NW1
                 NE = NE1
                 SW = SW1
                 SE = SE1 }),
          Node({ NW = NW2
                 NE = NE2
                 SW = SW2
                 SE = SE2 }) ->
            Node(
                { NW = map2 f NW1 NW2
                  NE = map2 f NE1 NE2
                  SW = map2 f SW1 SW2
                  SE = map2 f SE1 SE2 }
            )
        | Node({ NW = NW; NE = NE; SW = SW; SE = SE }), l ->
            Node(
                { NW = map2 f NW l
                  NE = map2 f NE l
                  SW = map2 f SW l
                  SE = map2 f SE l }
            )
        | l, Node({ NW = NW; NE = NE; SW = SW; SE = SE }) ->
            Node(
                { NW = map2 f l NW
                  NE = map2 f l NE
                  SW = map2 f l SW
                  SE = map2 f l SE }
            )
        | Leaf x, Leaf y -> Leaf(f x y)

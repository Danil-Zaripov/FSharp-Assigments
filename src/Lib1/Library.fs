module Trees

type MyTree<'a> =
    | Node of MyTree<'a> list
    | Leaf of 'a

module MyTree =
    let toLeaf x = Leaf x

    let nodeFromList xs = Node(List.map toLeaf xs)

    let emptyNode () =
        failwith "The tree contained an empty node"

    let rec map f =
        function
        | Node([]) -> emptyNode ()
        | Node(xs) -> Node(List.map (map f) xs)
        | Leaf x -> Leaf(f x)

    let rec fold folder st =
        function
        | Node([]) -> emptyNode ()
        | Node(xs) -> List.fold (fun acc x -> (fold folder acc x)) st xs
        | Leaf x -> folder st x

    let rec foldBack folder tr st =
        match tr with
        | Node([]) -> emptyNode ()
        | Node(xs) -> List.foldBack (fun x acc -> (foldBack folder x acc)) xs st
        | Leaf x -> folder st x

    let height tr =
        let rec _h cur =
            function
            | Node([]) -> emptyNode ()
            | Node(xs) -> List.fold (fun st x -> max st (_h (cur + 1) x)) -1 xs
            | Leaf _ -> cur

        _h 1 tr

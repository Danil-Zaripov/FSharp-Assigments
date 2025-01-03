module Trees

open FSharpPlus.Data


type MyTree<'a> =
    | Node of MyTree<'a> NonEmptyList
    | Leaf of 'a

module MyTree =
    let toLeaf x = Leaf x


    let ofList =
        function
        | x :: xs -> Node(NonEmptyList.create x xs)
        | [] -> invalidArg "list" "List was empty"

    let rec map f =
        function
        | Node(xs) -> Node(NonEmptyList.map (map f) xs)
        | Leaf x -> Leaf(f x)

    let rec fold folder st =
        function
        | Node(xs) -> NonEmptyList.fold (fun acc x -> (fold folder acc x)) st xs
        | Leaf x -> folder st x

    let sum tr = fold (+) 0 tr

    let rec foldBack folder tr st =
        match tr with
        | Node(xs) -> NonEmptyList.foldBack (fun x acc -> (foldBack folder x acc)) xs st
        | Leaf x -> folder st x

    let height tr =
        let rec _h cur =
            function
            | Node(xs) -> xs |> NonEmptyList.map (_h (cur + 1)) |> NonEmptyList.max
            | Leaf _ -> cur

        _h 1 tr

    let leafNumber tr = fold (fun st _ -> st + 1) 0 tr


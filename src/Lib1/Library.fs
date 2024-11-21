module Learning

type MyList<'a> =
    | Cons of 'a * MyList<'a>
    | Empty

    member this.IsEmp =
        match this with
        | Empty -> true
        | _ -> false

let inline (=.=) hd tl = Cons(hd, tl)

module MyList =

    let rec fold folder st =
        function
        | Cons(hd, tl) -> fold folder (folder st hd) tl
        | Empty -> st

    let rev xs = fold (fun st x -> x =.= st) Empty xs

    let toFSList xs =
        xs |> (fold (fun st x -> x :: st) []) |> List.rev

    let fromFSList xs =
        xs |> (List.fold (fun st x -> x =.= st) Empty) |> rev

    let bind alg xs = xs |> fromFSList |> alg |> toFSList

    let reduce folder =
        function
        | Cons(hd, tl) -> fold folder hd tl
        | Empty -> failwith "Applied reduce to an empty list"

    let rec filter f =
        function
        | Cons(hd, tl) -> if f hd then Cons(hd, filter f tl) else filter f tl
        | Empty -> Empty

    let length xs = fold (fun st _ -> st + 1) 0 xs

    let head =
        function
        | Cons(hd, _) -> hd
        | Empty -> failwith "Empty list ain't got no head"

    let tail =
        function
        | Cons(_, tl) -> tl
        | Empty -> failwith "Empty list ain't got no tail"

    let exactlyOne =
        function
        | Cons(hd, Empty) -> hd
        | _ -> failwith "The list did not only contain 1 element"

    let splitAt index list =
        let rec _splitAt acc =
            function
            | _, Empty -> rev acc, Empty
            | 0, rest -> rev acc, rest
            | n, Cons(x, xs) -> _splitAt (x =.= acc) (n - 1, xs)

        _splitAt Empty (index, list)

    let splitInto n list =
        let rec split acc n list =
            match n, list with
            | 0, _ -> rev acc
            | _, Empty -> rev acc
            | _, _ ->
                let size = ((length list) + n - 1) / n
                let part, rest = splitAt size list
                split (part =.= acc) (n - 1) rest

        split Empty n list

    let append list1 list2 =
        let rec _append acc =
            function
            | Empty -> acc
            | Cons(x, xs) -> _append (x =.= acc) xs

        rev (_append (rev list1) list2)

let inline (@.@) xs ys = MyList.append xs ys

module ListSorts =
    let bubbleSort lst =
        let rec _bubbleSort =
            function
            | Cons(p1, Cons(p2, rest)) ->
                if p1 > p2 then
                    p2 =.= (_bubbleSort (p1 =.= rest))
                else
                    p1 =.= (_bubbleSort (p2 =.= rest))
            | rest -> rest

        MyList.fold (fun acc _ -> _bubbleSort acc) lst lst

    let rec quickSort =
        function
        | Empty -> Empty
        | Cons(x, xs) ->
            let smaller = MyList.filter ((>) x) xs
            let bigger = MyList.filter ((<=) x) xs
            (quickSort smaller) @.@ (x =.= (quickSort bigger))

    let rec mergeSort lst =
        let rec _merge =
            function
            | (Cons(x, xs), Cons(y, ys)) ->
                if x < y then
                    x =.= (_merge (xs, y =.= ys))
                else
                    y =.= (_merge (x =.= xs, ys))
            | (xs, ys) -> if xs.IsEmp then ys else xs

        match lst with
        | Cons(p1, Cons(p2, rest)) ->
            let l1, l2 =
                let chunks = lst |> MyList.splitInto 2
                let p1, p2 = (chunks |> MyList.head, chunks |> MyList.tail |> MyList.exactlyOne)
                p1, p2

            _merge (mergeSort l1, mergeSort l2)
        | _ -> lst

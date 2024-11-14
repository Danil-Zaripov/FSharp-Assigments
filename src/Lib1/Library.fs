module Learning

module ListSorts =
    let bubbleSort lst =
        let rec _bubbleSort =
            function
            | p1 :: p2 :: rest ->
                if p1 > p2 then
                    p2 :: (_bubbleSort (p1 :: rest))
                else
                    p1 :: (_bubbleSort (p2 :: rest))
            | rest -> rest

        List.fold (fun acc _ -> _bubbleSort acc) lst lst

    let rec quickSort =
        function
        | [] -> []
        | x :: xs ->
            let smaller = List.filter ((>) x) xs
            let bigger = List.filter ((<=) x) xs
            (quickSort smaller) @ x :: (quickSort bigger)

    let rec mergeSort lst =
        let rec _merge =
            function
            | (x :: xs, y :: ys) ->
                if x < y then
                    x :: (_merge (xs, y :: ys))
                else
                    y :: (_merge (x :: xs, ys))
            | (xs, ys) -> max xs ys // max([], lst) = lst // for any lst

        match lst with
        | p1 :: p2 :: rest ->
            let l1, l2 =
                let chunks = lst |> List.splitInto 2
                let p1, p2 = (chunks |> List.head, chunks |> List.tail |> List.exactlyOne)
                p1, p2

            _merge (mergeSort l1, mergeSort l2)
        | _ -> lst

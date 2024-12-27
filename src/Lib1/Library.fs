﻿module Learning

module ListSorts =

    let rec insertSorted =
        function
        | (x :: xs, v) when v > x -> x :: (insertSorted (xs, v))
        | (lst, v) -> v :: lst

    let rec insertionSort =
        function
        | [] -> []
        | x :: xs -> insertSorted ((insertionSort xs), x)


    let bubbleSort lst =
        let rec sortUtil acc rev lst =
            match lst, rev with
            | [], true -> acc |> List.rev
            | [], false -> acc |> List.rev |> sortUtil [] true
            | x :: y :: tl, _ when x > y -> sortUtil (y :: acc) false (x :: tl)
            | hd :: tl, _ -> sortUtil (hd :: acc) rev tl

        sortUtil [] true lst


    let rec quickSort =
        function
        | [] -> []
        | x :: xs ->
            let smaller = List.filter ((>) x) xs
            let bigger = List.filter ((<=) x) xs
            (quickSort smaller) @ x :: (quickSort bigger)



    let rec mergeSort lst =
        let merge l1 l2 =
            let rec _merge lst1 lst2 res =
                match lst1, lst2 with
                | [], [] -> List.rev res
                | [], hd :: tl
                | hd :: tl, [] -> _merge [] [] ((List.rev (hd :: tl)) @ res)
                | x :: xs, y :: ys ->
                    if (x < y) then
                        _merge (xs) (y :: ys) (x :: res)
                    else
                        _merge (x :: xs) (ys) (y :: res)

            _merge l1 l2 []

        match lst with
        | p1 :: p2 :: rest ->
            let l1, l2 =
                let chunks = lst |> List.splitInto 2
                let p1, p2 = (chunks |> List.head, chunks |> List.tail |> List.exactlyOne)
                p1, p2

            merge (mergeSort l1) (mergeSort l2)
        | _ -> lst

module ArraySorts =
    let swap (left: 'a byref) (right: 'a byref) =
        let temp = left
        left <- right
        right <- temp

    let bubbleSort (arr: 'a array) =
        let len = arr |> Array.length

        for i in 0 .. len - 1 do
            for j in 1 .. len - 1 do
                if arr[j] < arr[j - 1] then
                    swap &arr[j] &arr[j - 1]

        arr

    let insertionSort (arr: 'a array) =
        let arr = Array.copy arr
        let n = arr.Length

        for i in 1 .. n - 1 do
            let key = arr[i]
            let mutable j = i - 1

            while j >= 0 && arr[j] > key do
                arr[j + 1] <- arr[j]
                j <- j - 1

            arr[j + 1] <- key

        arr

    let quickSort (arr: 'a array) =
        let partition (arr: 'a array) low high =
            let pivot = arr[high]

            let mutable i = low - 1

            for j in low .. high - 1 do
                if arr[j] < pivot then
                    i <- i + 1

                    swap &arr[i] &arr[j]

            swap &arr[i + 1] &arr[high]
            arr, i + 1

        let rec _quickSort (arr: 'a array) low high =
            if low < high then
                let (arr, piv) = partition arr low high
                let arr = _quickSort arr low (piv - 1)
                let arr = _quickSort arr (piv + 1) high
                arr
            else
                arr

        _quickSort arr 0 (arr.Length - 1)


    let divide_two_halves (left, right) =
        let length = right - left + 1 // end-inclusive
        let half_length = length / 2

        (left, left + half_length - 1), (left + half_length, right)

    let mergeSort arr =
        let arr = arr |> Array.copy

        let _merge (arr1: 'a array) (arr2: 'a array) =
            let final = Array.zeroCreate (arr1.Length + arr2.Length)
            let mutable fp = 0
            let mutable sp = 0

            for i in 0 .. final.Length - 1 do
                if fp >= arr1.Length then
                    final[i] <- arr2[sp]
                    sp <- sp + 1
                elif sp >= arr2.Length then
                    final[i] <- arr1[fp]
                    fp <- fp + 1
                elif arr1[fp] < arr2[sp] then
                    final[i] <- arr1[fp]
                    fp <- fp + 1
                else
                    final[i] <- arr2[sp]
                    sp <- sp + 1

            final

        let rec _mergeSort (arr: 'a array) =
            if arr.Length <= 1 then
                arr
            else
                let (f1, f2), (s1, s2) = divide_two_halves (0, arr.Length - 1)
                _merge (_mergeSort arr[f1..f2]) (_mergeSort arr[s1..s2])

        _mergeSort arr

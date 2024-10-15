namespace Learning

module Sorts =
    let swap (left: 'a byref) (right: 'a byref) =
        let temp = left
        left <- right
        right <- temp

    let ascendingOrderCompare a b = a < b
    let descendingOrderCompare a b = b < a

    let bubbleSortGeneral (arr: 'a array) compare =
        let arr = arr |> Array.copy

        let len = arr.Length

        for i in 0 .. len - 1 do
            for j in 1 .. len - 1 do
                if compare arr[j] arr[j - 1] then
                    swap &arr[j - 1] &arr[j]

        arr

    let quickSortGeneral (arr: 'a array) compare =
        let arr = arr |> Array.copy

        let partition (arr: 'a array) low high =
            let pivot = arr[high]

            let mutable i = low - 1

            for j in low .. high - 1 do
                if compare arr[j] pivot then

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

    let mergeSortGeneral arr compare =
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
                elif compare arr1[fp] arr2[sp] then
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

    let ascendingSort (sort: 'a array -> ('a -> 'a -> bool) -> 'a array) arr = ascendingOrderCompare |> sort arr

    let descendingSort (sort: 'a array -> ('a -> 'a -> bool) -> 'a array) arr = descendingOrderCompare |> sort arr

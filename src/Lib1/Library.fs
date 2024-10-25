namespace Learning

module Sorts =
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
    
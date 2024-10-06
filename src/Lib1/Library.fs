namespace Learning


module Sorts =
    let swap (left: 'a byref) (right: 'a byref) =
        let temp = left
        left <- right
        right <- temp

    let bubbleSort (arr: 'a array) = 
        let len = arr.Length

        for i in 0 .. len - 1 do
            for j in 1 .. len - 1 do
                if arr[j - 1] > arr[j] then
                    swap &arr[j-1] &arr[j]

        arr
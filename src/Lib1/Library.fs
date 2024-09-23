﻿namespace Learning

module Basic =

    let swap (left : 'a byref) (right : 'a byref) =
        let temp = left
        left <- right
        right <- temp

    let fib n = 
        let rec _fib p1 p2 cnt = 
            if cnt = 1 then p2
            else _fib p2 (p1 + p2) (cnt - 1)
        _fib 0 1 n


    let bubbleSort (arr: 'a array) =
        let len = arr |> Array.length
        for i in 0 .. len - 1 do
            for j in 1 .. len - 1 do
                if arr[j] < arr[j - 1] then
                    swap &arr[j] &arr[j - 1]
        arr
    
    let factorial n = 
        let product = List.fold (fun st x -> x * st) 1
        product [1..n]

    let rec funcBubbleSort (arr: 'a list) = 
        let rec _f arr = // Puts into place the greatest element that is not sorted
            match arr with
                | p1 :: p2 :: rest ->
                    if p1 > p2 then 
                        p2 :: ((p1 :: rest) |> funcBubbleSort)
                    else 
                        p1 :: ((p2 :: rest) |> funcBubbleSort)
                | _ -> arr

        List.fold (fun acc _ -> _f acc) arr arr // applies the function above to the list arr.Length times

    let bubbleSort1 arr =
        arr |> Array.toList |> funcBubbleSort |> List.toArray


 module Matrix = 
    let f<'a> x y = 
        (x, y)


    let product2_2 (mat1: int array2d) (mat2: int array2d) =
        array2D [|
            [|
                mat1[0,0] * mat2[0, 0] + mat1[0,1] * mat2[1,0]
                mat1[0,0] * mat2[0, 1] + mat1[0,1] * mat2[1,1]
            |];
            [|
                mat1[1,0] * mat2[0,0] + mat1[1,1] * mat2[1,0]
                mat1[1,0] * mat2[0,1] + mat1[1,1] * mat2[1,1]
            |]
        |]
    
    let sqrMat mat = 
        product2_2 mat mat

    let toBinary n = 
        let rec _f st n = 
            if n = 1 then 
                1 :: st
            else _f (n % 2 :: st) (n / 2)

        _f List.empty n |> List.toArray


    let Qmatrix = 
        array2D [[1;1];[1;0]]

    let identityMatrix = 
        array2D [[1;0];[0;1]]

    let formArrayOfMatrices n = 
        let mutable arr: (int array2d) array = Array.init n (fun _ -> Array2D.init 2 2 (fun _ _ -> 0))
       
        arr[0] <- Qmatrix

        for i in 1..n - 1 do 
            arr[i] <- sqrMat arr[i-1]

        arr

    let fib n =
        let inclusion = n |> toBinary |> Array.rev |> Array.map (fun x -> if x > 0 then true else false) 
        let array_of_mat = inclusion.Length |> formArrayOfMatrices
        
        let _folder st mat incl = 
            if incl then product2_2 st mat
            else st

        let final_matrix = Array.fold2 _folder identityMatrix array_of_mat inclusion

        final_matrix[0,1] // 0,0 is next fib; 0,1 and 1,0 is current fib; 1,1 is previous fib
    
    
module QuickSort =

    let partition (arr: 'a array) low high = 
        let pivot = arr[high]

        let mutable i = low - 1;

        for j in low .. high - 1 do 
            if arr[j] < pivot then 
                i <- i + 1

                Basic.swap &arr[i] &arr[j]

        Basic.swap &arr[i + 1] &arr[high]
        arr, i + 1

    let quickSort (arr: 'a array) = 
        let rec _quickSort (arr: 'a array) low high = 
            if low < high then 
           
               let (arr, piv) = partition arr low high

               let arr = _quickSort arr low (piv - 1) 
               let arr = _quickSort arr (piv + 1) high
               arr
            else arr

        _quickSort arr 0 (arr.Length - 1)
﻿namespace Lib1

module Say =
    let fib n = 
        let rec _fib p1 p2 cnt = 
            if cnt = 1 then p2
            else _fib p2 (p1 + p2) (cnt - 1)
        _fib 0 1 n


    let bubbleSort (arr: 'a array) =
        let mutable arr = arr
        let len = arr |> Array.length
        for i in 0 .. len - 1 do
            for j in 1 .. len - 1 do
                if arr[j] < arr[j - 1] then
                    let c = arr[j]
                    arr[j] <- arr[j-1]
                    arr[j-1] <- c
        arr
    
    let factorial n = 
        let product = List.fold (fun st x -> x * st) 1
        product [1..n]
 
        
 module Matrix = 
    let empty = 0
    
    let product2_2 (mat1: 'a array2d) (mat2: 'a array2d) =
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

    let powi n p = 
        let mutable ret = 1
        for i in 1 .. p do
            ret <- ret * n
        ret
   
    let pow2 = powi 2
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

        final_matrix[0,1]
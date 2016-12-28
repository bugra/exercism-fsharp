module AccumulateProgram


let accumulate fn input = 
    let rec accumulate' acc = function
        | []    -> acc
        | head::tail -> accumulate' (fn head :: acc) tail
    
    accumulate' [] input 
    |> List.rev
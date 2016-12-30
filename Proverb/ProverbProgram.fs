module ProverbProgram

let WORD_SEQUENCE = ["nail";"shoe";"horse";"rider";"message";"battle";"kingdom"]

let getPairOfWord aList n = 
    aList
    |> List.pairwise
    |> List.item (n - 1)
    

let line = function
    | number when number < 7 -> 
        sprintf "For want of a %s the %s was lost." <|| getPairOfWord WORD_SEQUENCE number
    | _ -> sprintf "And all for the want of a horseshoe nail."

let proverb =
    [1 .. 7]
    |> List.map line
    |> String.concat "\n"
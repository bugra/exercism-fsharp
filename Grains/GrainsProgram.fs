module GrainsProgram


let square (number:int) = 
    bigint (pown 2.0 (number-1))


let total = 
    [1..64]
    |> List.sumBy square
    
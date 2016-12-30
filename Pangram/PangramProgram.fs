module PangramProgram

let alphabet = ['a'..'z'] |> Set.ofList
let getLettersAsASet (input: string) = 
    input.ToLower() 
    |> Set.ofSeq

let isPangram (input: string) =
    Set.isSubset alphabet (getLettersAsASet input)
    
module MatrixProgram


let fromString (input: string) =
  input.Split([|'\n'|])
  |> Array.map (fun x -> x.Split([|' '|]))
  |> Array.map (Array.map int)


let transpose matrix =
    let rec transpose' acc (matrix':int list list) =
      match matrix'.Head.Length with 
      | 0 -> acc |> List.rev
      | _ -> transpose' ([for row in matrix' -> row.Head]::acc) (List.map (fun row -> match row with [] -> [] | h::t -> t) matrix')
    transpose' [] matrix


let rows anArray = 
  anArray


let cols anArray = 
  Array.map List.ofArray anArray
  |> List.ofArray
  |> transpose 
  |> List.map Array.ofList 
  |> Array.ofList
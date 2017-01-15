module SeriesProgram

open System


let convertInt (charArray: char []) =
  charArray
  |> Array.map (System.Char.ToString >> int)


let slices (input: string) n = 
  if n > input.Length then
    failwith "Number of characters exceed the length of string"

  input
  |> Seq.windowed n
  |> List.ofSeq
  |> List.map convertInt
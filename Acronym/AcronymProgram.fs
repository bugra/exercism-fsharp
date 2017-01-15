module AcronymProgram

open System.Text.RegularExpressions


let getAcronym input = 
  let letters = 
    Regex.Split(input, "[^\w]") 
    |> Seq.map (fun x -> x.ToUpperInvariant())
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.map (fun x -> x.[0])
    |> Array.ofSeq

  new string(letters)


let acronym = function
  | "" -> ""
  | input -> getAcronym input





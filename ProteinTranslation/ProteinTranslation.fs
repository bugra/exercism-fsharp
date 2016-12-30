module ProteinTranslationProgram

let codonToProtein = function
  | "AUG" -> "Methionine"
  | "UUU" | "UUC" -> "Phenylalanine"
  | "UUA" | "UUG" -> "Leucine"
  | "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
  | "UAU" | "UAC" -> "Tyrosine"
  | "UGU" | "UGC" -> "Cysteine"
  | "UGG" -> "Tryptophan"
  | "UAA" | "UAG" | "UGA" -> "STOP"
  | _ -> failwith "Not valid codon"


let translate (codon:string) = 
  codon
  |> Seq.chunkBySize 3
  |> Seq.map (fun (chars: char []) -> new string [|for c in chars -> c|])
  |> Seq.map codonToProtein
  |> Seq.takeWhile (fun x -> x <> "STOP")
  |> Seq.toList    
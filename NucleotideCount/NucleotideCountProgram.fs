module NucleotideCountProgram


let isNucleotide ch= 
    Seq.contains ch "ACTG"

let count item aSeq =
    match item with 
    | ch when isNucleotide ch -> 
        aSeq
        |> Seq.filter(fun x -> x = ch)
        |> Seq.length
    | _ -> failwith  "It is not a nucleotide" 

let countNucleotides strand =
    "ATCG"
    |> Seq.map (fun item -> item, count item strand)
    |> Seq.toList
    |> Map.ofSeq

let nucleotideCounts item = 
    match item with 
    | "" -> [ ( 'A', 0 ); ( 'T', 0 ); ( 'C', 0 ); ( 'G', 0 ) ] |> Map.ofList
    | strand  -> countNucleotides strand

module AnagramProgram


let isAnagram (first: string) (second: string) =
  (first.ToLowerInvariant() |> Seq.sort |> List.ofSeq) = (second.ToLowerInvariant() |> Seq.sort |> List.ofSeq) && (first.ToLowerInvariant() <> second.ToLowerInvariant()), second


let anagrams words word = 
  words
  |> List.map (isAnagram word)
  |> List.filter fst
  |> List.map snd
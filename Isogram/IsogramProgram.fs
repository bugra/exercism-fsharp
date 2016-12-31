module IsogramProgram

open System

let normalizeWord (word: string) = 
  word.ToLowerInvariant()
  |> Seq.filter (Char.IsLetterOrDigit)

let isogram (word: string) = 
  let normalizedWord = word |> normalizeWord
  normalizedWord |> Set.ofSeq |> Set.count = (Seq.length normalizedWord)


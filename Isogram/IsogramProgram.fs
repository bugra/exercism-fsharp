module IsogramProgram

open System

let normalizeWord (word: string) = 
  word.ToLowerInvariant()
  |> Seq.filter (Char.IsLetterOrDigit)
  |> Array.ofSeq
  |> System.String

let isogram (word: string) = 
  let normalizedWord = word |> normalizeWord
  normalizedWord |> Set.ofSeq |> Set.count = (String.length normalizedWord)


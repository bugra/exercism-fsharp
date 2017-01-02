module WordCountProgram


open System.Text

let normalize (word: string) =
  word.Replace(",", " ").Replace("!&@$%:,", "").ToLowerInvariant()

let split (item:string) =
  item.Split([|' '|]) 

let wordCount (phrase: string) =
  RegularExpressions.Regex.Replace(phrase, "[^(\w|\')]", " ")
  |> normalize
  |> split
  |> Seq.countBy (fun s -> s.Trim())
  |> Map.ofSeq
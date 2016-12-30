module KinderGartenGardenProgram


open System


type Plant =
  | Clover
  | Radishes
  | Grass
  | Violets

// Type Aliasing
type Child = string
type Garden = Map<Child,Plant seq>

// Pass the type hint to guard against different types in the list
let (childrenList: Child list) = 
  [
    "Alice"
    "Bob"
    "Charlie"
    "David"
    "Eve"
    "Fred"
    "Ginny"
    "Harriet"
    "Ileana" 
    "Joseph" 
    "Kincaid"
    "Larry"
  ]

let split (input: string) = 
  input.Split([|'\n'|], StringSplitOptions.None)

let parse = function
  | 'C' -> Clover
  | 'R' -> Radishes
  | 'G' -> Grass
  | 'V' -> Violets
  | unk -> failwithf "Unknown plant: %c" unk

let garden children description = 
  description
    |> split
    |> Seq.map ((Seq.map parse) >> (Seq.chunkBySize 2))
    |> Seq.collect (Seq.zip children)
    |> Seq.groupBy fst
    |> Seq.map (fun (child, grouping) -> 
      (child, grouping |> Seq.collect snd))
    |> Map.ofSeq

let defaultGarden = garden childrenList

let lookupPlants child garden = 
  defaultArg (garden |> Map.tryFind child) Seq.empty    
module BinarySearchProgram


let addToOption number optional  =
  match optional with 
  | Some value -> Some (value + number)
  | _ -> None
  
    
let rec binarySearch anArray value = 
  match anArray with
  | [||] -> None
  | _ ->
    let half = (anArray.Length / 2) 
    match value with 
    | value when value = anArray.[half] -> Some half
    | value when value < anArray.[half] -> binarySearch anArray.[0..half-1] value
    | value -> 
      let maxIndex = anArray.Length - 1
      binarySearch anArray.[half+1..maxIndex] value |> addToOption (half + 1)

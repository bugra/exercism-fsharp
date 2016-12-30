module PhoneNumberProgram

open System

let digitize number =
  number 
  |> String.filter Char.IsDigit

let parsePhoneNumber phoneNumber = 
  match digitize phoneNumber with
  | number when String.length number = 10 -> Some number
  | number when String.length number = 11 ->
      match [for c in number -> c] with
      | head :: tail when head = '1' -> Some (tail |> Array.ofList |> System.String)
      | _ -> None
  | _ -> None
  
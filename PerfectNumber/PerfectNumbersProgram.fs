module PerfectNumbersProgram

type NumberType =
  | Deficient
  | Perfect
  | Abundant


let half number = 
  match number % 2 with
  | 1 -> (number + 1) / 2
  | 0 -> number / 2
  | _ -> number


let factors number =
  [1 .. (half number)]
  |> List.filter (fun n -> number % n = 0)


let sumOfFactors number = 
  factors number
  |> List.sum


let classify number = 
  match sumOfFactors number with 
  | sum when sum = number -> NumberType.Perfect
  | sum when sum > number -> NumberType.Abundant
  | _ -> NumberType.Deficient
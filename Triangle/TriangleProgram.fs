module TriangleProgram

let pairs aList = 
  let rec combinations acc size set = seq {
    match size, set with 
    | 0, [] -> yield acc 
    | _, [] -> () 
    | n, x::xs -> 
        if n > 0 then yield! combinations (x::acc) (n - 1) xs
        if n >= 0 then yield! combinations acc n xs 
  }

  combinations [] 2 aList


let invalidTriangle first second third =
  (third >= (first + second)) || (first >= (second + third)) || (second >= (first + third)) ||
  (first = 0m) || (second = 0m) || (third = 0m)


type TriangleKind = 
  | Equilateral
  | Isosceles
  | Scalene


let countEquality aList =
  aList
  |> List.map (fun ls ->
    match ls with
      | [] -> (1m, 2m)
      | first::second -> first, second.[0]
  )
  |> List.filter (fun (x, y) -> x = y)
  |> List.length


let kind (first:decimal) (second:decimal) (third:decimal) = 
  if invalidTriangle first second third then
    raise  (System.InvalidOperationException("Triange is not valid"))
  let pairOfSides = pairs [first; second; third] |> Seq.toList
  match countEquality pairOfSides with 
    | 3 -> TriangleKind.Equilateral
    | 1 -> TriangleKind.Isosceles
    | _ -> TriangleKind.Scalene    
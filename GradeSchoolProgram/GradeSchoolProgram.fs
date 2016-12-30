module GradeSchoolProgram

let mutable empty = Map.empty<string, int>

let add x y (school: Map<string, int>) = 
    school.Add(x, y)

let getFirstItems aList =
    aList
    |> List.map fst


let roster school=
    school
    |> Map.toList
    |> List.groupBy snd
    |> List.map (fun (x, y) -> (x, getFirstItems y))
    |> List.sortBy fst


let grade number school =
    school
    |> Map.toSeq
    |> Seq.filter (fun (x, y) -> y = number)
    |> Seq.map (fun (x, y) -> x)    
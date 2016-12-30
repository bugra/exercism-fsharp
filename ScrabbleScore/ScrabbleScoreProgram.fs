module ScrabbleScoreProgram


let FIRST_SET = set ['A'; 'E'; 'I'; 'O'; 'U'; 'L'; 'N'; 'R'; 'S'; 'T']
let SECOND_SET = set ['D'; 'G']
let THIRD_SET = set ['B'; 'C'; 'M'; 'P'] 
let FOURTH_SET = set ['F'; 'H'; 'V'; 'W'; 'Y']
let FIFTH_SET = set ['K']
let SIXTH_SET = set ['J'; 'X']
let SEVENTH_SET = set ['Q'; 'Z']


let mapLetterToScore = function
  | letter when Set.contains letter FIRST_SET -> 1 
  | letter when Set.contains letter SECOND_SET -> 2 
  | letter when Set.contains letter THIRD_SET -> 3
  | letter when Set.contains letter FOURTH_SET -> 4
  | letter when Set.contains letter FIFTH_SET -> 5
  | letter when Set.contains letter SIXTH_SET -> 8
  | letter when Set.contains letter SEVENTH_SET -> 10
  | _ -> 0

let score (input:string) =
  input.ToUpperInvariant()
  |> Seq.sumBy mapLetterToScore



    
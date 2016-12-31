module QueenAttackProgram


let canAttack firstQueenPosition secondQueenPosition = 
  match firstQueenPosition, secondQueenPosition with 
  | first, second when first = second -> 
    failwith "Both pieces cannot be put into the same place"
  | (x1, y1), (x2, y2) -> x1 = x2 || y1 = y2 || abs(x1-x2) = abs(y1-y2)

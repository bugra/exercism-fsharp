module QueenAttackProgram


let canAttack firstTuple secondTuple = 
  match firstTuple, secondTuple with 
  | firstTuple, secondTuple when firstTuple = secondTuple -> 
    failwith "Both pieces cannot be put into the same place"
  | (x1, y1), (x2, y2) -> x1 = x2 || y1 = y2 || abs(x1-x2) = abs(y1-y2)

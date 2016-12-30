module Seq
    
                            
let keep predicate aSequence = 
  [
   for ii in aSequence do 
   if ii |> predicate then yield ii
  ]

let discard predicate aSequence = keep (predicate >> not) aSequence
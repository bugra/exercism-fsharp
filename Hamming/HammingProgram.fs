module HammingProgram


let compute first second =
    Seq.zip first second
    |> Seq.map (fun (x, y) -> if x = y then 0 else 1)
    |> Seq.sum
    
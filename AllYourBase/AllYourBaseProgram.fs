module AllYourBaseProgram


let computeBase number newBase = 
  // We need to come up with a plan to find out the largest exponent of a base
  // then do the conversion where the base case is base is larger than the number itself
  let rec computeBase' number newBase acc = 
    match number with
    | number when number < newBase -> number :: acc
    | number when number = newBase -> 1 :: acc
    | number when number > newBase -> computeBase' (number % newBase) newBase ((number / newBase):: acc)
    | _ -> failwith "Error!"

  computeBase' number newBase [] |> List.rev


let getLargestPower (number: int) (newBase: int) = 
  Seq.initInfinite (fun n -> n + 1)
  |> Seq.takeWhile (fun index -> (float newBase) ** (float index) <= (float number))
  |> Seq.max


let rebase (inputBase: int) inputDigits outputBase =
  let totalValue = 
    inputDigits
    |> List.rev
    |> List.mapi (fun i x -> (float inputBase) ** (float i) * (float x))

  totalValue



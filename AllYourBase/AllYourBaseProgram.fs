module AllYourBaseProgram


let getLargestPower (number: int) (newBase: int) = 
  Seq.initInfinite (fun n -> n + 1)
  |> Seq.takeWhile (fun index -> (float newBase) ** (float index) <= (float number))
  |> Seq.max


let computeBase number newBase = 
  // We need to come up with a plan to find out the largest exponent of a base
  // then do the conversion where the base case is base is larger than the number itself
  let largestPower = getLargestPower number newBase
  /// We need to consume the number through its exponent application
  [largestPower .. -1 .. 1]
  // TODO: Need to consume the number while we are passing the number into a list
  // TODO: take a look at List.mapFold, there must be a function that does this job already
  // TODO: ideally, this can be a recursive function but since we know the total number in a list
  // TODO: it may not make a lot of sense to follow through on that one
  (*
  let rec computeBase' number newBase acc = 
    match number with
    | number when number < newBase -> number :: acc
    | number when number = newBase -> 1 :: acc
    | number when number > newBase -> computeBase' (number % newBase) newBase ((number / newBase):: acc)
    | _ -> failwith "Error!"

  computeBase' number newBase [] |> List.rev

  *)


let rebase (inputBase: int) inputDigits outputBase =
  let totalValue = 
    inputDigits
    |> List.rev
    |> List.mapi (fun i x -> (float inputBase) ** (float i) * (float x))

  totalValue



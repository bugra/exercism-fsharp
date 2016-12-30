module SieveProgram


let isDivisibleBy number x =
  number % x = 0

let sieve aList = 
  let rec sieve' aList primeNumbers =
    match aList with
    // Base case when it is empty, primeNumbers are whatever the remaining in the primeNumbers list
    | [] -> primeNumbers 
    | head :: tail -> 
      // Check if the remaining numbers can be divisible by other numbers
      let remaining = tail |> List.filter (fun number -> (isDivisibleBy number head |> not))
      sieve' remaining (head :: primeNumbers)

  sieve' aList [] |> List.rev

let primesUpTo n = sieve [2 .. n]
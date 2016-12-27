module SumOfMultiplesProgram


let isDivisibleBy dividend divisor =
    divisor % dividend = 0

// .. is inclusive in both boundaries
let findDivisiblesBy number divisor =
    [1 .. number-1]
    |> List.filter (isDivisibleBy divisor)

// List.collect is similar to flatMap in Scala
let sumOfMultiples multiples number =
    multiples
    |> List.map (findDivisiblesBy number)
    |> List.collect id
    |> List.distinct
    |> List.sum
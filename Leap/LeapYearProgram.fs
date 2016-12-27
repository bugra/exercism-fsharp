module LeapYearProgram

  let isDivisibleBy dividend divisor  =
    dividend % divisor = 0

  let isLeapYear year = 
    if isDivisibleBy year 400 then
      true
    elif (isDivisibleBy year 100 && not (isDivisibleBy year 400)) then
      false 
    else 
      isDivisibleBy year 4

  let pprint (year: int) =
    match isLeapYear year with 
    | true -> sprintf "%i is leap year" year 
    | false -> sprintf "%i is not a leap year" year


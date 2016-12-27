module BobProgram 

let isUpperCase input = 
    input
    |> String.filter (fun c -> System.Char.IsWhiteSpace(c) |> not)
    |> String.filter (fun c -> System.Char.IsLetterOrDigit(c))
    |> String.forall (fun c -> System.Char.IsUpper(c))

let isQuestion (input: string) = 
    (isUpperCase(input) |> not) && input.[input.Length - 1] = '?'

let isShouting input = 
    input 
    |> isUpperCase

let isExcited input = 
    String.exists (fun c -> c = '!') input || isShouting input
    
let isWhiteSpace input = 
    String.forall (fun c -> System.Char.IsWhiteSpace(c)) input

let hey input =
    match input with 
    | input when isWhiteSpace input -> "Fine. Be that way!"
    | input when isQuestion input -> "Sure."
    | input when isExcited input -> "Whoa, chill out!"
    | input -> "Whatever."
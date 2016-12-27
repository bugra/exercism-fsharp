module GigaSecondProgram

open System

let [<Literal>] GIGA_SECONDS = 1000000000.0

let gigasecond (input: DateTime) =
    input.AddSeconds(GIGA_SECONDS).Date


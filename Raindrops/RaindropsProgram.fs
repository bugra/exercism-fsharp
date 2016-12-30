module RaindropsProgram

let rainDropSpeak = function
    | 3 -> "Pling"
    | 5 -> "Plang"
    | 7 -> "Plong"
    | number -> sprintf "%i" number


let pow number item  = 
    let mutable acc = 0
    let rec pow' number item = 
        if number % item = 0 && number / item <> 0 then  
            acc <- acc + 1
            pow' (number/item) item
        else
            acc
    pow' number item


let countfactors factors number =
    let numbers = 
        factors 
        |> Seq.filter (fun factor -> (pow number factor) <> 0 )
        |> Seq.toList

    if Seq.isEmpty numbers then
        [number]
    else
        numbers


let convert number =
    countfactors [3; 5; 7] number
    |> Seq.map(rainDropSpeak)
    |> String.concat ""
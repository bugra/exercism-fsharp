module RNATranscriptionProgram


let transcription = function
        | 'G' -> Some 'C'
        | 'C' -> Some 'G'
        | 'T' -> Some 'A'
        | 'A' -> Some 'U'
        | _   -> None

let toRna input = 
    input 
    |> Seq.choose transcription
    
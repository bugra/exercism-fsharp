module SecretHandShakeProgram


type Secrets =
    | ReverseSecret
    | Secret of string


let secrets =
   [
     (ReverseSecret, 16)
     (Secret "jump", 8)
     (Secret "close your eyes", 4)
     (Secret "double blink", 2)
     (Secret "wink", 1)
   ]


let appendSecrets completeSecrets secret =
  match secret with
  | ReverseSecret -> completeSecrets |> List.rev
  | Secret secret -> secret :: completeSecrets


let filterSecret number secret = 
    secret &&& number <> 0


let handshake number = 
    secrets
    |> List.filter (snd >> filterSecret number)
    |> List.map fst
    |> List.fold appendSecrets []
    |> List.rev
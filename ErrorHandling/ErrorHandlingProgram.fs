module ErrorHandlingProgram    


type Result<'a> =
    | Ok of 'a
    | Error of string

let handleErrorByThrowingException (): System.Exception =
  raise (System.Exception("Exception"))

let handleErrorByReturningOption input = 
  match input |> System.Int32.TryParse with 
  | true, x -> Some x
  | _ -> None

let handleErrorByReturningResult input = 
  match input |> System.Int32.TryParse with 
  | true, x -> Ok x
  | _ -> Error "Could not convert input to integer"

let bind f x =
  match x with 
  | Ok x -> f x
  | error -> error

let cleanupDisposablesWhenThrowingException r =
  use resource = r
  raise <| System.Exception("Resource cleaned up before throwing exception")    
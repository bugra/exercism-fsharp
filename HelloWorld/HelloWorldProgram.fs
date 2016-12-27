module HelloWorldProgram
    
let hello input =  
  match input with
  | Some item -> sprintf "Hello, %s!" item
  | None -> "Hello, World!"
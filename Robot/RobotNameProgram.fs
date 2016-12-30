module RobotNameProgram    

let ALPHABET = ['a' .. 'z']

let RNG = System.Random()

type Robot = {
    Name: string option
}

let generateName() = 
  
  let digits = 
    [1 .. 3]
    |> List.map (fun _ -> sprintf "%i" (RNG.Next(0, 9)))

  let letters = 
    [1 .. 2]
    |> List.map (fun _ -> sprintf "%c" ALPHABET.[RNG.Next(0, ALPHABET.Length - 1)])
  
  letters @ digits
  |> String.concat ""
  
let name robot =
  robot.Name.Value

let mkRobot() = 
  {Robot.Name = Some (generateName ())}

let reset robot =
  {robot with Name=Some (generateName())}    
module RobotSimulatorProgram


type Bearing = 
  | North 
  | East 
  | South
  | West 


type Position = int * int

type Robot = 
  {
    Direction: Bearing
    Position: Position
  }


type MovementType =
  | Advance
  | Left
  | Right


let mapMovementType = function
  | 'R' -> MovementType.Right
  | 'L' -> MovementType.Left
  | 'A' -> MovementType.Advance
  | _ -> failwith "Cannot recognize the movement type"


let createRobot dir pos =
  {
    Robot.Direction=dir
    Robot.Position=pos
  }


let turnRight (robot: Robot) =
  let newDir = 
    match robot.Direction with
    | Bearing.North -> Bearing.East
    | Bearing.East -> Bearing.South
    | Bearing.South -> Bearing.West
    | Bearing.West -> Bearing.North

  {robot with Direction=newDir}


let turnLeft (robot: Robot) =
  let newDir = 
    match robot.Direction with
    | Bearing.North -> Bearing.West
    | Bearing.East -> Bearing.North
    | Bearing.South -> Bearing.East
    | Bearing.West -> Bearing.South

  {robot with Direction=newDir}


let advance (robot: Robot) =
  let newPos = 
    match robot.Direction, robot.Position with
    | Bearing.North, pos -> ((fst pos) , snd pos + 1)
    | Bearing.East, pos -> ((fst pos) + 1, (snd pos) )
    | Bearing.South, pos -> ((fst pos) , snd pos - 1)
    | Bearing.West, pos -> ((fst pos) - 1, (snd pos) )

  {robot with Position=newPos}


let simulate (robot:Robot) input =
  let mutable simulatedRobots = [|robot|]
  input
  |> Seq.map mapMovementType
  |> Seq.iteri (fun index movementType ->
      let (newRobot: Robot) = 
        match movementType with
        | MovementType.Right -> turnRight simulatedRobots.[index]
        | MovementType.Left -> turnLeft simulatedRobots.[index]
        | MovementType.Advance -> advance simulatedRobots.[index]

      simulatedRobots <- Array.append simulatedRobots [|newRobot|]
  )

  simulatedRobots.[simulatedRobots.Length - 1]
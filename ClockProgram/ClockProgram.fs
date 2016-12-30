module ClockProgram


let [<Literal>] MINUTES_IN_ONE_DAY = 1440


type Clock = {
    Hour: int
    Minute: int
} 

let isNegative number = 
    number <> abs(number)

let compute hours minutes =
    let mutable totalMinutes = (hours * 60) + minutes

    if isNegative totalMinutes then
        totalMinutes <- totalMinutes + MINUTES_IN_ONE_DAY
    let intermediateHour = totalMinutes / 60
    let (intermediateHour, minutes) = (totalMinutes / 60, totalMinutes % 60)
    let hours = intermediateHour % 24
    hours, minutes


let mkClock hour minute = 
    let hours, minutes = compute hour minute
    {
         Clock.Hour = hours
         Minute = minutes
     }


let display clock = 
    sprintf "%02i:%02i" clock.Hour clock.Minute


let subtract minutes clock = 
    let totalMinutes = clock.Minute - minutes
    let hours, minutes = compute clock.Hour totalMinutes
    mkClock hours minutes

let add minutes clock = 
    let totalMinutes = clock.Minute + minutes
    let hours, minutes = compute clock.Hour totalMinutes
    mkClock hours minutes
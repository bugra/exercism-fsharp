module SpaceAgeProgram

open System

type Planet = 
    | Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
        
                
let SECONDS_IN_A_YEAR = 31557600m

let convertYears seconds =
    seconds / SECONDS_IN_A_YEAR

let findCoefficientBasedOnPlanet = function
    | Planet.Earth -> 1.0m
    | Planet.Mercury -> 0.2408467m
    | Planet.Venus -> 0.61519726m
    | Planet.Mars -> 1.880815m
    | Planet.Jupiter -> 11.862615m
    | Planet.Saturn -> 29.447498m
    | Planet.Uranus -> 84.016846m
    | Planet.Neptune -> 164.79132m

let spaceAge planet (seconds:decimal) =
    let coefficient = findCoefficientBasedOnPlanet planet
    Math.Round((convertYears  seconds) / coefficient, 2)



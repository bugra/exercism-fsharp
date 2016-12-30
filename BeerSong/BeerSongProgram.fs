module BeerSongProgram


let [<Literal>] EMPTY_BEER_STRING = """No more bottles of beer on the wall, no more bottles of beer.
Go to the store and buy some more, 99 bottles of beer on the wall.
"""

let [<Literal>] ONE_BEER_STRING = """1 bottle of beer on the wall, 1 bottle of beer.
Take it down and pass it around, no more bottles of beer on the wall.
"""

let [<Literal>] TWO_BEER_STRING = """2 bottles of beer on the wall, 2 bottles of beer.
Take one down and pass it around, 1 bottle of beer on the wall.
"""

let verse = function
    | 0 -> EMPTY_BEER_STRING
    | 1 -> ONE_BEER_STRING
    | 2 -> TWO_BEER_STRING
    | number -> sprintf """%i bottles of beer on the wall, %i bottles of beer.
Take one down and pass it around, %i bottles of beer on the wall.""" number number (number - 1) + "\n"


let verseStrings upper lower = 
    [upper .. -1 .. lower]
    |> List.map verse
    |> String.concat "\n"

let verses upper lower =
    (verseStrings upper lower) + "\n"    


let sing = verses 99 0 
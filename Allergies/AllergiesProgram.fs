module AllergiesProgram


type Allergen =
  | Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats


let allergicTo (item: Allergen) amount = 
    match item, amount with
    | _, 0 -> false
    | Eggs, number -> number >= 1
    | Peanuts, number -> number >= 2
    | Shellfish, number -> number >= 4
    | Strawberries, number -> number >= 8
    | Tomatoes, number -> number >= 16
    | Chocolate, number -> number >= 32
    | Pollen, number -> number >= 64
    | Cats, number -> number >= 64


let rec parseAllergens number acc = 
  match number with 
  | number when number >= 128 -> parseAllergens (number % 128) (Allergen.Cats :: acc)
  | number when number <128 && number >= 64 -> parseAllergens (number % 64) (Allergen.Pollen :: acc)
  | number when number <64 && number >= 32-> parseAllergens (number % 32) (Allergen.Chocolate :: acc)
  | number when number <32 && number >= 16-> parseAllergens (number % 16) (Allergen.Tomatoes :: acc)
  | number when number <16 && number >= 8-> parseAllergens (number % 8) (Allergen.Strawberries :: acc)
  | number when number <8 && number >= 4 -> parseAllergens (number % 4) (Allergen.Shellfish :: acc)
  | number when number <4 && number >= 2-> parseAllergens (number % 2) (Allergen.Peanuts :: acc)
  | number when number = 1 -> parseAllergens 0 (Allergen.Eggs :: acc)
  | number when number = 0 -> acc 
  | _ -> sprintf "Number: %i cannot be smaller than 0" number |> failwith



let allergies = function
  | 0 -> []
  | number -> parseAllergens number []
module SimpleLinkedListProgram

type LinkedList<'a> = 
  | Head of 'a * LinkedList<'a>
  | Nil


let nil = Nil


let isNil = function 
  | Nil -> true
  | _ -> false


let datum = function
  | Head(item, _) -> item
  | _ -> failwith "This is Nil, you cannot pass!"


let create head tail =
  Head(head, tail)


let next = function
  | Head(_, tail) -> tail
  | _ -> failwith "This is Nil, there is no next!"


let toList linkedList = 
  let rec toList' acc linked = 
    match linked with 
    | Nil -> acc |> List.rev
    | Head(item, tail) -> toList' (item::acc) tail
  toList' [] linkedList


let reverse linkedList = 
  let rec rev' acc linked = 
    match linked with
    | Nil -> acc
    | Head(head, tail) -> rev' (create head acc) tail
  rev' Nil linkedList


let fromList aList = 
  let rec fromList' acc aList' =
    match aList' with 
    | [] -> acc |> reverse
    | head::tail -> fromList' (create head acc) tail
  fromList' Nil aList






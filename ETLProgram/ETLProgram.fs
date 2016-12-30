module ETLProgram


let transform input =
    input
    |> Map.toList
    |> List.map (fun (cnt, y) -> 
        match y with 
        | [] -> []
        | aNumberOfItems -> List.map (fun (item: string) -> (item.ToLower(), cnt)) aNumberOfItems)
    |> List.collect id
    |> Map.ofSeq    
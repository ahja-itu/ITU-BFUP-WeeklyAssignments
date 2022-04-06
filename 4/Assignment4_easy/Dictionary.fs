module Dictionary


    type Dict = D of Set<string>
    

    let empty() = D Set.empty<string>

    let insert word (D dict) = Set.add word dict |> D

    let lookup word (D dict) =
        Set.contains word dict
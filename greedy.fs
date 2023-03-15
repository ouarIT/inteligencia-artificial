namespace busqueda

module greedy =
    let estrategia h =
        {
            vacia = Map.empty
            insertar = fun pqueue x -> Map.add(h x, x.estado) x pqueue
            remover = fun pqueue -> 
                        match Map.tryFindKey (fun _ _ -> true) pqueue with
                        | Some k -> Some (Map.find k pqueue, Map.remove k pqueue)
                        | None -> None                        
        }
    
    let key h n = n.estado, h n
        
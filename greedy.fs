namespace busqueda

module greedy =
    let estrategia = 
    {
        vacia = Map.empty
        insertar = fun pqueue x -> Map.add (key x) x.estado pqueue
        remover = fun pqueue -> 
                    match Map.tryFindKey (fun _ _ -> true) pqueue with
                    | Some k -> Some (Map.find k pqueue, Map.remove k pqueue)
                    | None -> None
    }
    
   let key n = n.estado, List.length (obtener_posibles_valores n.estado n.i n.j)
namespace busqueda

module costouniforme =
    let estrategia =
        {
            vacia = Map.empty
            insertar = fun pqueue x -> Map.add(x.costo_ruta, x.estado) x pqueue
            remover = fun pqueue -> 
                        match Map.tryFindKey (fun _ _ -> true) pqueue with
                        | Some k -> Some (Map.find k pqueue, Map.remove k pqueue)
                        | None -> None                        
        }
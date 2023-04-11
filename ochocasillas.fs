namespace busqueda

module OchoCasillas =
    type accion =
        | Left
        | Right
        | Up
        | Down
    type estado = List<int>

    let inicio = [ 7; 2; 4; 5; 0; 6; 8; 3; 1 ]
    let goal = [ 1; 2; 3; 4; 5; 6; 7; 8; 0 ]
    let costo _ _ _ = 1.0

    let meta estado =
        List.map2 (fun x y -> (x,y)) goal estado
        |> List.forall (fun (x,y) -> x = y)

    let cero estado =
        List.findIndex (fun x -> x = 0) estado

    
    let sucesores estado =
        let indice = cero estado
        [
            sucesor indice Left estado
            sucesor indice Right estado
            sucesor indice Up estado
            sucesor indice Down estado
        ]   |> List.choose id

    let problema estado = {
        inicio = estado
        sucesores = sucesores
        meta = meta
        costo = costo
    }

    let h1 nodo = 
        List.zip goal nodo.estado
        |> List.sumBy (fun (x,y) -> if x <> y && x <> 0 then 1.0 else 0.0)

    // lets build h2 using manhattan distance
    //let h2 nodo =

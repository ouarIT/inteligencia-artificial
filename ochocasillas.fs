namespace busqueda

module OchoCasillas =
    type accion =
        | Left
        | Right
        | Up
        | Down
    type estado = int[]
(*   type estado2 = int[][]
    type estado3 = list<int>
    type estado4 = int*int*int*int*int*int*int*int*int*int
*) 
    let inicio = [| 7; 2; 4; 5; 0; 6; 8; 3; 1 |]
    let goal = [| 1; 2; 3; 4; 5; 6; 7; 8; 0 |]
    let costo _ _ _ = 1.0

    let meta estado =
        Array.map2 (fun x y -> (x,y)) goal estado
        |> Array.forall (fun (x,y) -> x = y)

    let cero estado =
        Array.findIndex (fun x -> x = 0) estado

    let sucesor i accion (estado : estado) =
        let swap i j =
            let estado' =
                [| for v in estado do v|]
            //let temp = estado.[i]
            estado'.[i] <- estado.[j]
            estado'.[j] <- estado.[i]
            estado'
        match accion with
        | Left -> if i % 2 <> 0
                    then Some (accion, swap i (i-1))
                    else None
        | Right -> if i % 3 <> 2
                    then Some (accion, swap i (i+1))
                    else None
        | Up -> if i > 2
                    then Some (accion, swap i (i-3))
                    else None
        | Down -> if i < 6
                    then Some (accion, swap i (i+3))
                    else None
    let sucesores estado =
        let indice = cero estado
        [
            sucesor indice Left estado
            sucesor indice Right estado
            sucesor indice Up estado
            sucesor indice Down estado
        ]   |> List.choose id
        
    let problema estado =
        { inicio = estado
          meta = meta
          costo = costo
          sucesores = sucesores
        }
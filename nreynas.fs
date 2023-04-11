namespace busqueda

module NReinas =
    type estado = int list
    let accion = ()
    let inicio n = 
        let rnd = System.Random()
        [1..n]
        |> List.map (fun _ -> rnd.Next(1, n+1))
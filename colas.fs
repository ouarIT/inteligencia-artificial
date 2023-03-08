namespace busqueda

type cola<'a> = list<'a>

module Cola =
    let queue cola x = cola @ [x]
    let dequeue cola = 
        match cola with
        | h :: t -> Some (h, t)
        | [] -> None
    
    let empty = []
    
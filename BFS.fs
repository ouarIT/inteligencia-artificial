namespace busqueda

module BFS = 
    open Cola
    let estrategia =
        {
            vacia = empty
            insertar = queue
            remover = dequeue
        }

    let key n = n.estado   



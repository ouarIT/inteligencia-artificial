namespace busqueda

module BFS = 
    open Cola
    let estrategia =
        {
            vacia = empty
            insertar = queue
            remover = dequeue
        }



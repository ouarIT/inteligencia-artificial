open busqueda

let estado = [3;6;0;4;2;8;5;7;1]


match Capitulo3.busquedaGrafo
    (astar.key OchoCasillas.h1)
    (astar.estrategia OchoCasillas.h1)
    (OchoCasillas.problema estado) with
| Some n -> let sol = Capitulo3.acciones n
            printfn "solucion con greedy: %A" sol
            // show deepness
            printfn "profundidad: %i" (List.length sol)
| None -> printfn "no hay solucion"

(*



// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"


// circle area
let circleArea r = 
    System.Math.PI * r * r

printf "area del ciruculo unitario = %f\n" (circleArea 1.0)


let rec factorial n =
    if n = 0 then 1
    else n * factorial (n-1)

printfn "factorial de %i = %i" 5 (factorial 5)

let rec power = function
    | (_, 0) -> 1.0
    | (x,n) -> x * power (x, n-1)

printfn "2^5 = %f" (power (2.0, 5))

let g n = n+4
printfn "g(5) = %i" (g 5)

let h (x,y) = 
    System.Math.Sqrt(x*x + y*y)

printfn "h(3,4) = %f" (h (3.0, 4.0))

let rec f n = 
    if n = 0 then 0
    else n + f(n-1)

printfn "f(4) = %i" (f 4)



let rec fib n = 
    if n = 0 then 0
    else if n = 1 then 1
    else fib(n-1) + fib(n-2)

printfn "fib(4) = %i" (fib 4)

let rec sum m n =
    if n = 0 then m
    else m + n + sum (m) (n-1)

printfn "sum(1,2) = %i" (sum 1 2)


let rec length = function
    | [] -> 0
    | x::xs -> 1 + length xs

printfn "length [1;2;3] = %i" (length [1;2;3])   

let rec delete n  y =
    match y with 
    | [] -> []
    | (x: 'a)::(xs: 'a list) -> if x = n then delete n xs else x::delete n xs
    
let rec delete' n  y =
    match y with 
    | [] -> []
    | (x: 'a)::(xs: 'a list) -> if x = n then xs else x::delete' n xs


printfn "delete 5 [1;2;3;5;4] = %A" (delete 5 [1;2;3;5;4])

let rec append list1 list2 =
    match list1 with
    | [] -> list2
    | x::xs -> x::append xs list2

printfn "append [1;2;3] [4;5;6] = %A" (append [1;2;3] [4;5])

let rec exists p = function
    | [] -> false
    | x::xs -> p x || exists p xs

printfn "exists (fun x -> x = 5) [1;2;3;5;4] = %A" (exists (fun x -> x = 5) [1;2;3;5;4])

let rec forall p = function
    | [] -> true
    | x::xs -> p x && forall p xs
printfn "forall (fun x -> x > 0) [1;2;3;5;4] = %A" (forall (fun x -> x > 0) [1;2;3;5;4])

// map : ('a -> 'b) -> 'a list -> 'b list
let rec map f = function
    | [] -> []
    | x::xs -> f x :: map f xs

printfn "map (fun x -> x + 1) [1;2;3;5;4] = %A" (map (fun x -> x + 1) [1;2;3;5;4])

*)
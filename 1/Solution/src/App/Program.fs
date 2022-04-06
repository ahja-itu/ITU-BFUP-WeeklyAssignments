
// Exercise 1.1
let sqr x = x * x

// Exercise 1.2
let pow x n = System.Math.Pow (x, n)

// Exercise 1.3
let rec sum =
    function
    | 0 -> 0
    | x -> x + sum (x - 1)
    
// Exercise 1.4
let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

// Exercise 1.5
let dup (s:string) = s + s

// Exercise 1.6
let rec dupn (s:string) (n:int) =
    match n with
    | 0 -> ""
    | _ -> s + (dupn s (n - 1))
    
// Exercise 1.7
let rec bin (n:int, k:int) =
    match n - k with
    | 0 -> 1
    | _ -> match k with
           | 0 -> 1
           | _ -> bin (n-1, k-1) + bin (n-1, k) 
    
// Exercise 1.8
let timediff (h1:int, m1:int) (h2:int, m2:int) =
    (h2 - h1) * 60 + (m2 - m1)


// Exercise 1.9
let minutes (h:int, m:int) =
    timediff (0, 0) (h, m)

// Exercise 1.10
let curry f x y = f (x, y)

let uncurry f (x, y) = f x y

//
// Assignment 1.11
// The scrabble thing
//

let empty (pair: char * int) (pos: int) = pair

// Assignment 1.12

// word er en kæde af funktioner med et gemt ord indeni

let add (newPos:int) (cv: char * int) (word: int -> char * int)
     = fun (pos: int) -> if newPos = pos then cv else word pos

// Assignment 1.13
let hello: int -> char * int
    = empty (char 0, 0)
        |> add 0 ('H', 4)
        |> add 1 ('E', 1) 
        |> add 2 ('L', 1) 
        |> add 3 ('L', 1) 
        |> add 4 ('O', 1)

// Assignment 1.14
let rec singleLetterScore : (int -> char * int) -> int -> int
    = fun word pos ->
        match word pos with
        | (_, n) -> n

let rec doubleLetterScore : (int -> char * int) -> int -> int
    = fun word pos -> singleLetterScore word pos |> (*) 2
        
let rec trippleLetterScore : (int -> char * int) -> int -> int
    = fun word pos -> singleLetterScore word pos |> (*) 3
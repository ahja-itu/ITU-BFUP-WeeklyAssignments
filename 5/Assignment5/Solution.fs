

// Exercise 5,1
let rec sum (m: int) (n:int) : int =
    let rec aux acc =
        function
        | n' when n' = n -> acc + n'
        | n' -> aux (acc + m + n') (n' + 1)
    aux m 0


// Exercise 5.2
let length (lst: 'a list) : int =
    let rec length' lst' c =
        match lst' with
        | [] -> c 0
        | x :: xs -> length' xs (fun r -> c (1 + r))
    length' lst id


// Exercise 5.3
let foldBack (f: 'a -> 'b -> 'b) (lst: 'a list) (acc: 'b) : 'b =
    let rec foldBack' lst' acc' c =
        match lst' with
        | [] -> c acc'
        | x :: xs -> foldBack' xs acc' (fun r -> c (f x r))
    foldBack' lst acc id

// Exercise 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

let factC x =
    let rec aux x' c =
        match x' with
        | 0 -> c 1
        | _ -> aux (x' - 1) (fun r -> c (r * x'))
    aux x id

// Exercise 5.5
let fibA (x:int) : int =
    let rec aux a b x' =
        match x' with
        | 0 -> a
        | _ -> aux b (a + b) (x' - 1)
    aux 0 1 x

let fibC (x: int) : int =
    let rec aux x' c =
        match x' with
        | 0 -> c 0
        | 1 -> c 1
        | _ -> aux (x' - 1) (fun r -> c r) + aux (x' - 2) (fun r -> c r)
    aux x id


// Exercise 5.6
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> c (1 :: res)) (n - 1)


// Exercise 5.7
type word = (char * int) list
type aExp =
    | N of int 
    | V of string 
    | WL 
    | PV of aExp 
    | Add of aExp * aExp 
    | Sub of aExp * aExp 
    | Mul of aExp * aExp 
    | CharToInt of cExp 
and cExp =
    | C  of char
    | CV of aExp
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

let rec arithEvalSimple (exp: aExp) (word: word) (state: Map<string, int>) : int =
    match exp with
    | N n -> n
    | CharToInt c -> int (charEvalSimple c word state)
    | _ -> failwith "not supported"

and charEvalSimple (exp: cExp) (word: word) (state: Map<string, int>) : char =
    match exp with
    | C c -> c
    | IntToChar aExp -> char (arithEvalSimple aExp word state)
    | _ -> failwith "not supported"

// Exercise 5.8
let rec arithEvalTail (exp: aExp) (word: word) (state: Map<string, int>) (c: int -> 'a) : 'a =
    match exp with
    | N n -> c n
    | CharToInt cExp -> charEvalTail cExp word state (fun r -> c (int r))
    | _ -> failwith "not supported"

and charEvalTail (exp: cExp) (word: word) (state: Map<string, int>) (c: char -> 'a) : 'a =
    match exp with
    | C ch -> c ch
    | IntToChar aExp -> arithEvalTail aExp word state (fun r -> c (char r))
    | _ -> failwith "not supported"

let arithEval a w s = arithEvalTail a w s id
let charEval c w s  = charEvalTail c w s id
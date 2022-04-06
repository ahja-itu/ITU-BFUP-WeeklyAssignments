

// Exercise 2.1
let rec downto1 n
    = if n <= 0 then [] else n :: downto1 (n - 1)

let rec downto2
    = function
      | n when n <= 0 -> []
      | n -> n :: downto2 (n - 1)

// Exercise 2.2
let removeOddIdx list =
    let rec aux = function
        | [], _ -> []
        | x :: xs, n when n % 2 = 0 -> x :: aux (xs, (n + 1))
        | _ :: xs, n -> aux (xs, (n + 1))
    aux (list, 0)

// Exercise 2.3
let rec combinePair = 
    function
    | [] -> []
    | _ :: [] -> []
    | a :: b :: xs -> (a, b) :: combinePair xs

// Exercise 2.4
type complex = (float * float)

let mkComplex: float -> float -> complex
    = fun a b -> complex (a, b)

let complexToPair : complex -> float * float
    = fun (a, b) -> (a, b)

let (|+|) : complex -> complex -> complex 
    = fun (a, b) (c, d) ->
        mkComplex (a + c) (b + d)

let (|*|) : complex -> complex -> complex
    = fun (a, b) (c, d) ->
        mkComplex (a * c - b * d) (b * c + a * d)

let (|-|) : complex -> complex -> complex
    = fun c1 (c, d) ->
        mkComplex -c -d |> (|+|) c1

let (|/|) : complex -> complex -> complex
    = fun c1 (a, b) -> mkComplex (a / (a ** 2.0 + b ** 2.0)) (-b / (a ** 2.0 + b ** 2.0)) 
                       |> (|*|) c1

// Exercise 2.5
let explode1: string -> char list
    = function
      | "" -> []
      | s -> s.ToCharArray () |> List.ofArray

let rec explode2: string -> char list
    = function
    | "" -> []
    | s -> s.[0] :: explode2 (s.Remove(0, 1))

// Exercise 2.6
let implode: char list -> string
    = function
      | [] -> ""
      | cs -> List.foldBack (fun c acc -> string c + acc) cs ""

let implodeRev : char list -> string
    = function
      | [] -> ""
      | cs -> List.rev cs |> implode

// Exercise 2.7
let toUpper: string -> string
    = explode1 >> List.map System.Char.ToUpper >> implode


// Exercise 2.8
// ack: (m, n)
let rec ack: int * int -> int
    = function
      | (0, n) -> n + 1
      | (m, 0) -> ack (m - 1, 1)
      | (m, n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
      | (_, _) -> raise (System.ArgumentException("Arguments to the ackermann function can't be negative"))

 // Exercise 2.9
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let timeArg1: ('a -> 'b) -> 'a -> 'b * System.TimeSpan
    = fun f a -> time (fun () -> f a)

// Exercise 2.10
// downto3 could perhaps be optimized such that we do one runthough of the list
// instead of two
let downto3: (int -> 'a -> 'a) -> int -> 'a -> 'a
     = fun f n e ->
        downto2 (n - 1)
        |> List.map (fun n' -> f n')
        |> List.fold (fun e' f' -> f' e') (f n e)

let fac: int -> int
    = function
      | 0 -> 1
      | n -> downto3 (*) n 1

let range: (int -> 'a) -> int -> 'a list
    = fun g n ->
        match n with
        | _ when n <= 0 -> []
        | _ -> downto3 (fun n' e -> (g n') :: e) n []

// Assignment 2.11
let hello: (char * int) list
    = List.zip (explode1 "HELLO") [4; 1; 1; 1; 1]

// Assignment 2.12
type word = (char * int) list
type squareFun = word -> int -> int -> int

let singleLetterScore: squareFun
    = fun word pos acc -> 
        match word.Item(pos) with
        | _, value -> value + acc

let doubleLetterScore: squareFun
    = fun word pos acc -> (2 * singleLetterScore word pos acc) - acc

let tripleLetterScore: squareFun
    = fun word pos acc -> (3 * singleLetterScore word pos acc) - (2 * acc)

let doubleWordScore: squareFun
    = fun _ _ acc -> acc * 2

let tripleWordScore: squareFun
    = fun _ _ acc -> acc * 3

let isConsonant: (char * int) -> bool
    = fun character -> 
        match character with
        | (c, _) -> match System.Char.ToLower(c) with
                    | 'a'|'e'|'i'|'o'|'u'|'æ'|'ø'|'å' -> false
                    | _ -> true
    
let oddConsonants: squareFun
    = fun word _ acc ->
        let flipIfConsonant acc' b = if b then not acc' else acc'
        let invertAccIfTrue x = if x then -acc else acc

        List.map isConsonant word
        |> List.fold flipIfConsonant false
        |> invertAccIfTrue

// Red exercises
type square = (int * squareFun) list

let SLS : square = [(0, singleLetterScore)];;
let DLS : square = [(0, doubleLetterScore)];;
let TLS : square = [(0, tripleLetterScore)];;
let DWS : square = SLS @ [(1, doubleWordScore)];;
let TWS : square = SLS @ [(1, tripleWordScore)];;


let calculatePoints : square list -> word -> int
    = fun squares word ->
        let aux = List.mapi (fun index square -> List.map (fun (priority, sqFun) -> (priority, sqFun word index)) square) squares
                  |> List.fold (fun acc t -> (List.fold (fun acc' t' -> t' :: acc') [] t) @ acc) []
                  |> List.sortBy (fun (priority, _) -> priority)
                  |> List.map (fun (_, sqFun) -> sqFun)
                  |> List.fold (fun acc sqFun -> sqFun << acc) id
        aux 0
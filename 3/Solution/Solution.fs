type aExp =
| N of int // Integer value
| V of string // Variable
| WL // Length of the word
| PV of aExp // Point value of character at specific word index
| Add of aExp * aExp // Addition
| Sub of aExp * aExp // Subtraction
| Mul of aExp * aExp // Multiplication


let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)
let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

// Assignment 3.1
let rec arithEvalSimple : aExp -> int
    = function
      | N n -> n
      | Add (e1, e2) -> (+) (arithEvalSimple e1) (arithEvalSimple e2)
      | Sub (e1, e2) -> (-) (arithEvalSimple e1) (arithEvalSimple e2)
      | Mul (e1, e2) -> (*) (arithEvalSimple e1) (arithEvalSimple e2)
      | _ -> failwith "Unsupported"

// Assignment 3.2
let binop f x y s = f (x s) (y s)

let rec arithEvalState : aExp -> Map<string, int> -> int
    = fun exp (* state *) -> 
        match exp with
        | V s -> fun state -> Map.tryFind s state |> Option.defaultValue 0
        | Add (e1, e2) -> binop (+) (arithEvalState e1) (arithEvalState e2)
        | Sub (e1, e2) -> binop (-) (arithEvalState e1) (arithEvalState e2)
        | Mul (e1, e2) -> binop (*) (arithEvalState e1) (arithEvalState e2)
        | _ -> fun _ -> arithEvalSimple exp

// Assignment 3.3
type word = (char * int) list
type squareFun = word -> int -> int -> int
let explode1: string -> char list
    = function
      | "" -> []
      | s -> s.ToCharArray () |> List.ofArray
let hello: (char * int) list
    = List.zip (explode1 "HELLO") [4; 1; 1; 1; 1]

let rec arithEval : aExp -> word -> Map<string, int> -> int
    = fun exp word (* state *) ->
        match exp with
        | WL -> fun _ -> List.length word
        | PV pv -> snd << word.get_Item << arithEval pv word
        | Add (e1, e2) -> binop (+) (arithEval e1 word) (arithEval e2 word)
        | Sub (e1, e2) -> binop (-) (arithEval e1 word) (arithEval e2 word)
        | Mul (e1, e2) -> binop (*) (arithEval e1 word) (arithEval e2 word)
        | _ -> arithEvalState exp

// Assignment 3.4
type cExp =
| C  of char      (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
| CV of aExp      (* Character lookup at word index *)

let rec charEval : cExp -> word -> Map<string, int> -> char
    = fun exp word (* state *) ->
        match exp with
        | C c -> fun _ -> c
        | ToUpper exp' -> System.Char.ToUpper << charEval exp' word
        | ToLower exp' -> System.Char.ToLower << charEval exp' word
        | CV exp' -> fst << word.get_Item << arithEval exp' word

// Assignment 3.5
type bExp =
| TT (* true *)
| FF (* false *)
| AEq of aExp * aExp (* numeric equality *)
| ALt of aExp * aExp (* numeric less than *)
| Not of bExp  (* boolean not *)
| Conj of bExp * bExp  (* boolean conjunction *)
| IsDigit of cExp  (* check for digit *)
| IsLetter of cExp  (* check for letter *)
| IsVowel of cExp  (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

// Initially copied from my assignment 2, but I've rewritten it a bit
let isConsonant': char -> bool
    = fun c -> 
        match System.Char.ToLower(c) with
        | 'a'|'e'|'i'|'o'|'u' -> false
        | _ -> true


let rec boolEval : bExp -> word -> Map<string, int> -> bool
    = fun exp word ->
        match exp with
        | TT -> fun _ -> true
        | FF -> fun _ -> false
        | AEq (leftExp, rightExp) -> binop (=) (arithEval leftExp word) (arithEval rightExp word)
        | ALt (leftExp, rightExp) -> binop (<) (arithEval leftExp word) (arithEval rightExp word)
        | Not exp' -> not << boolEval exp' word
        | Conj (leftExp, rightExp) -> binop (&&) (boolEval leftExp word) (boolEval rightExp word)
        | IsDigit exp' -> System.Char.IsDigit << charEval exp' word
        | IsLetter exp' -> System.Char.IsLetter << charEval exp' word
        | IsVowel exp' -> not << isConsonant' << charEval exp' word

let isConsonant : cExp -> bExp
    = Not << IsVowel


// Assignment 3.6
type stmnt =
| Skip
| Ass of string * aExp
| Seq of stmnt * stmnt
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt       (* while statement *)

let invertAcc: Map<string, int> -> Map<string, int>
    = fun state -> 
        Map.tryFind "_acc_" state 
        |> Option.defaultValue 0 
        |> fun acc -> Map.add "_acc_" -acc state |> Map.add "_result_" -acc

let setResToAcc: Map<string, int> -> Map<string, int>
    = fun state ->
        Map.tryFind "_acc_" state
        |> Option.defaultValue 0
        |> fun res -> Map.add "_result_" res state

// Assignment 3.7
let rec evalStmnt : stmnt -> word -> Map<string, int> -> Map<string, int>
    = fun statement word (* state *) ->
        match statement with
        | Skip -> id
        | Ass (name, arithExp) ->
            fun state ->
                Map.add name (arithEval arithExp word state) state
        | Seq (leftStatement, rightStatement) ->
            evalStmnt leftStatement word >> evalStmnt rightStatement word
        | ITE (boolExp, trueStatement, falseStatement) ->
            fun state ->
                match boolEval boolExp word state with
                | true -> evalStmnt trueStatement word state
                | false -> evalStmnt falseStatement word state
        | While (boolExp, stm) ->
            fun state ->
                match boolEval boolExp word state with
                | true -> evalStmnt stm word state
                          |> evalStmnt (While (boolExp, stm)) word
                | false -> state

// Assignment 3.8
// squareFun = word -> pos -> acc -> INT value of points calculated
let stmntToSquareFun : stmnt -> squareFun
    = fun stm ->
        fun word pos acc ->
            evalStmnt stm word (Map [("_pos_", pos); ("_acc_", acc)]) |> Map.find "_result_"

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers =
  stmntToSquareFun
    (Seq (Ass ("_result_", V "_acc_"),
          While (V "i" .<. WL,
                 ITE (IsDigit (CV (V "i")),
                      Seq (
                           Ass ("_result_", V "_result_" .*. N -1),
                           Ass ("i", WL)),
                      Ass ("i", V "i" .+. N 1)))))

// Assignment 3.9
// See OddConsonants statement implementation around line 177
let oddConsonants: stmnt =
    Seq (
        Seq (
            Ass ("i", N 0),
            While ((V "i" .<. WL),
                Seq (
                    ITE (Not (IsVowel (CV (V "i"))),
                        Ass ("_acc_", V "_acc_" .*. N -1),
                        Skip),
                    Ass ("i", V "i" .+. N 1)
                )
            )
        ),
        Ass ("_result_", V "_acc_")
    )

// Assignment 3.10
type square = (int * squareFun) list
type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

// From assignment 2
let calculatePoints: square list -> word -> int
    = fun squares word ->
        let aux = List.mapi (fun index square -> List.map (fun (priority, sqFun) -> (priority, sqFun word index)) square) squares
                  |> List.fold (fun acc t -> (List.fold (fun acc' t' -> t' :: acc') [] t) @ acc) []
                  |> List.sortBy (fun (priority, _) -> priority)
                  |> List.map (fun (_, sqFun) -> sqFun)
                  |> List.fold (fun acc sqFun -> sqFun << acc) id
        aux 0

let square2TosquareType: square2 -> square
    = List.map (fun (p, s) -> (p, stmntToSquareFun s))

let calculatePoints2: square2 list -> word -> int
    = fun squares (* word *) -> 
        List.map square2TosquareType squares
        |> fun sqs -> calculatePoints sqs (* word *)


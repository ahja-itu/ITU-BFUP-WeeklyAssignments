﻿module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b> =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> =
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s -> Success ((), {s with vars = List.tail s.vars}))


    let wordLength : SM<int> =
        S (fun s -> Success (List.length s.word, s))

    type PairChoice = Fst | Snd
        
    let characterValue (pos : int) : SM<char> =
        S (fun s ->
            match List.tryItem pos s.word with
            | Some (a, _) -> Success (a, s)
            | None -> Failure (IndexOutOfBounds pos))

    let pointValue (pos : int) : SM<int> =
        S (fun s ->
            match List.tryItem pos s.word with
            | Some (_, b) -> Success (b, s)
            | None -> Failure (IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> =
        let aux (lst: Map<string, int> list) : Result<Map<string, int> list, Error> =
                match lst with
                // The following case is not "precise", since VarNotFound is an error for a different
                // situation, but this is the closest match..
                | []              -> Failure (VarNotFound var)
                | state :: states ->
                    match Map.tryFind var state with
                    | Some _ -> Failure (VarExists var)
                    | None   -> Success ((Map.add var 0 state) :: states)
        
        S (fun s ->
            match Set.contains var s.reserved with
            | true  -> Failure (ReservedName var)
            | false -> match aux s.vars with
                       | Success lst           -> Success ((), {s with vars = lst})
                       | Failure typeOfFailure -> Failure typeOfFailure)
        
    let update (var : string) (value : int) : SM<unit> =
        let rec aux (lst: Map<string, int> list) (acc: Map<string, int> list) : Result<Map<string, int> list, Error> =
            match lst with
            | []      -> Failure (VarNotFound var)
            | x :: xs ->
                match Map.tryFind var x with
                | Some _ -> Success ((List.rev acc) @ [(Map.add var value x)] @ xs)
                | None -> aux xs (x :: acc)
        
        S (fun s ->
            match s.vars with
            | [] -> Failure (VarNotFound var)
            | _  ->
                match aux s.vars [] with
                | Success vars' -> Success ((), {s with vars = vars'})
                | Failure f -> Failure f)
              

    
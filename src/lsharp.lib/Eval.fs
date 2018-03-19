module LSharp.Eval

open LSharp.Core

// let (|LookupType|_|) name = 
//     match System.Type.GetType(name) with
//     | null -> None
//     | t -> Some t

let evalSymbol s =
    match Symbols.tryFindGlobal s with
    | Some s -> Ok s
    | _ -> Error(sprintf "Unable to resolve symbol: %A" s)
    // | _ ->
    //     match s with
    //     | LookupType t -> Ok ( Type t )
    //     //| Simple "add" -> Ok ( Object ( { Invoke = add } ))
    //     | _ -> Error(sprintf "Unable to resolve symbol: %A" s)

let evalList eval (list : Form list) =
    let evalHead head =
        match head with
        | Symbol sym ->
            match Symbols.tryFind SpecialForms.tryFind sym with
            | Some var -> Ok var
            | None -> Error "Unable to resolve symbol"
        | _ -> eval head

    match list with
    | [] -> Ok (List [])
    | head :: args -> 
        evaluate {
            let! head' = evalHead head
            return!
                match head' with
                | SpecialForm fn -> fn eval args
                | Macro (ma,mb) -> Macro.callMacro eval (ma,mb) args
                | _ -> Error "Not a function"
        }

let evalVector eval (vec : Form array) =
    let vec' = Array.map eval vec
    match Array.tryFind hasError vec' with
    | Some (Error err) -> Error err
    | _ ->
        Ok (Vector (Array.choose result vec'))

let rec eval (form : Form) =
    match form with
    | Symbol s -> evalSymbol s
    | Quote f -> Ok f
    | List l -> evalList eval l
    | Vector v -> evalVector eval v
    | _ -> Ok form

let evalAll (forms : Form list) =
    let rec evalR l forms =
        match forms with
        | [] -> Ok (List.rev l)
        | f :: rest ->
            match eval f with
            | Ok res -> evalR (res :: l) rest
            | Error err -> Error err
    evalR [] forms
    


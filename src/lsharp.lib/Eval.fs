module LSharp.Core.Eval

let add args =
    match args with
    | [ Number(a); Number(b) ] -> Ok (Number(a + b))
    | _ -> Error "Wrong arguments"

let evalSymbol s =
    match s with
    | "add" -> Ok ( Object ( { Invoke = add } ))
    | _ -> Error( "Unknown symbol")

let evalList eval (fn :: args) =
    let fn = eval fn
    
    match fn with
    | Ok (Object o) ->
        match o with 
        | :? Fn as f -> f.Invoke args
        | _ -> Error "Not a function"
    | _ -> Error "Not a function"

let rec eval (form : Form) =
    match form with
    | Symbol s -> evalSymbol s
    | List ([]) -> Ok form
    | List l -> evalList eval l
    | _ -> Ok form

let rec evalAll (forms : Form list) =
    match forms with
    | [] -> Ok Empty
    | [f] -> eval f
    | f :: rest ->
        match eval f with
        | Ok _ -> evalAll rest
        | err -> err


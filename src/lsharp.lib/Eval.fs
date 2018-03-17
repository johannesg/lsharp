module LSharp.Eval

open LSharp.Core
open LSharp.SpecialForms

open System.Collections.Immutable
open System.Reflection

let (|LookupType|_|) = function
|  name ->
    match System.Type.GetType(name) with
    | null -> None
    | t -> Some t
| _ -> None

let evalSymbol s =
    match Symbols.tryFind s with
    | Some s -> Ok s
    | _ ->
        match s with
        | LookupType t -> Ok ( Type t )
        //| Simple "add" -> Ok ( Object ( { Invoke = add } ))
        | _ -> Error(sprintf "Unable to resolve symbol: %A" s)

//let invokeObject eval (obj : obj) args =
//    let eargs = List.map eval args
//    let argErr = List.tryFind hasError eargs
//    match (obj, argErr) with 
//    | (:? Fn as f, None) ->
//        f.Invoke (List.choose result eargs)
//    | (_, Some err) -> err
//    | _ -> Error "Not a function"

let invokeFn eval fn args =
    evaluate {
        let! fn = eval fn
        return!
            match fn with
            | Fn f ->
                Ok (String ("Executing function"))
            | _ ->
                Error ("Could not evaluate to a function")
    }
   
 
let evalList eval (list : Form list) =
    match list with
    | [] -> Ok (List [])
    | head :: args -> 
        evaluate {
            let! head' = eval head
            return!
                match head' with
                | Fn fn -> fn eval args
                | _ -> Error "Not a function"
        }
            //evaluate {
            //    let! fn = eval fn
            //    let! res =
            //        match fn with
            //        | (Symbol (Dot)) ->
            //            invokeDot eval args 
            //        | (Object o) ->
            //            invokeObject eval o args
            //        | _ -> Error "Not a function"

            //    return res
            //}
    //let fn = eval fn

    //match fn with
    //| Ok (Symbol (Dot)) ->
    //    invokeDot eval args 
    //| Ok (Object o) ->
    //    invokeObject eval o args
    //| Error err -> Error err
    //| _ -> Error "Not a function"

let rec eval (form : Form) =
    match form with
    | Symbol s -> evalSymbol s
    | Quote f -> Ok f
    | List l -> evalList eval l
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
    


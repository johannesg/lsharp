module LSharp.Eval

open LSharp.Core

open System.Collections.Immutable
open System.Reflection


type EvalBuilder() =
    member this.Bind(m, f) = Result.bind f m
        //match m with
        //| Ok res -> f res
        //| Error err -> Error err

    member this.Return(x) = Ok x
    member this.ReturnFrom(x) = x

let hasError = function
| Ok _ -> false
| Error _ -> true

let result = function
| Ok res -> Some res
| Error _ -> None

let evaluate = new EvalBuilder()

let formToObject = function
| Number x -> Ok (x :> obj)
| Object o -> Ok o
| _ -> Error "Failed to cast form to object"

let evalToObject eval f =
    eval f |> Result.bind formToObject

let bindMap f list =
    let l = List.map f list
    let argErr = List.tryFind hasError l
    match argErr with
    | Some (Error err) -> Error err
    | None -> Ok (List.choose result l)
    | _ -> Error "Unknown"

let getMethodInfo (t : System.Type) m =
    match t.GetMethod(m) with
    | null -> Error(sprintf "Method %s not found" m)
    | mi -> Ok mi

let mapArgument eval ((arg : Form), (pi : ParameterInfo)) =
    evaluate {
    let! a = eval arg
    return!
        if pi.ParameterType = typeof<Form> then
            Ok (a :> obj)
        else
            formToObject a
    }

let mapArguments eval args (mi : MethodInfo) =
    let prms = mi.GetParameters()

    if prms.Length <> List.length args then
        Error ("Wrong number of arguments")
    else
        let p = List.zip args (Array.toList prms)
        bindMap (mapArgument eval) p

let invokeStatic eval (t : System.Type) m (args : Form list) =
    printf "Invoking method: %A on type %A" m t.FullName
    evaluate {
        //let! args' = bindMap (evalToObject eval) args
        let! mi = getMethodInfo t m

        let! args' = mapArguments eval args mi

        return Object (mi.Invoke(null, List.toArray args'))
    }

//let invokeDot eval (args : Form list) =
//    let getInvoker = function
//    | [] -> Error ("No arguments to dot")
//    | t :: args ->
//        //Ok (t, args)
//        evaluate {
//            let! i = eval t
//            return (i, args)
//        }

//    evaluate {
//        let! (invoker, args') = getInvoker args

//        let! res = 
//            match (invoker, args') with
//            | (Type t, Symbol (Simple method) :: args'') ->
//                invokeStatic eval t method args''
//            | _ -> Error ("Bad invoke")

//        return res
//    }
        

    //match args with
    //| (Type t) :: [] -> 
    //    Error "Type needs a method"
        
    //| (Type t) :: method :: args -> 
    //    Ok (Symbol(Bool(true)))
    //| _ -> Error "Invoke must have a type"

//let (|LookupType|_|) = function
//| Simple name ->
//    match System.Type.GetType(name) with
//    | null -> None
//    | t -> Some t
//| _ -> None

//let evalSymbol s =
//    match s with
//    | Bool b -> Ok (Object b)
//    | Nil -> Ok (Object null)
//    | LookupType t -> Ok ( Type t )
//    //| Simple "add" -> Ok ( Object ( { Invoke = add } ))
//    | _ -> Error(sprintf "Unable to resolve symbol: %A" s)

let (|SpecialForm|_|) form =
    match form with
    | Symbol s -> 
        match s with
        | "." -> Some Dot
        | "def" -> Some Def
        | "defmacro" -> Some DefMacro
        | "fn" -> Some SpecialForm.Fn
        | _ -> None
    | _ -> None
    

let invokeObject eval (obj : obj) args =
    let eargs = List.map eval args
    let argErr = List.tryFind hasError eargs
    match (obj, argErr) with 
    | (:? Fn as f, None) ->
        f.Invoke (List.choose result eargs)
    | (_, Some err) -> err
    | _ -> Error "Not a function"

let invokeSpecial eval f args =
    Ok (String (sprintf "Execute special: %A" f))

let evalList eval (list : Form list) =
    match list with
    | [] -> Ok (List [])
    | fn :: args -> 
        match fn with
        | SpecialForm f ->
            invokeSpecial eval f args
        | _ ->
            evaluate {
                let! fn = eval fn
                return!
                    match fn with
                    | Fn f ->
                        Ok (String ("Executing function"))
                    | _ ->
                        Error ("Could not evaluate to a function")
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
    //| Symbol s -> evalSymbol s
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
    


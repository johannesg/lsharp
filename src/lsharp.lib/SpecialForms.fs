module LSharp.SpecialForms

open LSharp.Core
open System.Reflection

// let (|SpecialForm|_|) form =
//     match form with
//     | Symbol s -> 
//         match s with
//         | "." -> Some Dot
//         | "def" -> Some Def
//         | "defmacro" -> Some DefMacro
//         | "fn" -> Some SpecialForm.Fn
//         | _ -> None
//     | _ -> None
    
//let getType args =
//    match args with
//    | [] -> Error "Wrong number of arguments"
//    | Symbol s :: args -> 

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


let invoke (eval : Form -> Result<Form, string>) (args : Form list) =
   let getInvoker = function
   | [] -> Error ("No arguments to dot")
   | t :: args ->
       //Ok (t, args)
       evaluate {
           let! i = eval t
           return (i, args)
       }

   evaluate {
       let! (invoker, args') = getInvoker args

       let! res = 
           match (invoker, args') with
           | (Type t, Symbol method :: args'') ->
               invokeStatic eval t method args''
           | _ -> Error ("Bad invoke")

       return res
   }

let def (eval : Form -> Result<Form, string>) (args : Form list) =
    match args with
    | [Symbol sym;form] ->
        evaluate {
            let! var = eval form
            Symbols.addGlobal sym var
            return (Bool true)
        }
    | [_;_] -> Error "First arg must be a symbol"
    | _ -> Error "Wrong number of arguments"

let defType (eval : Form -> Result<Form, string>) (args : Form list) =
    match args with
    | [String fullName] ->
        let typeName :: _ = List.ofArray (fullName.Split(','))
        match System.Type.GetType(fullName) with
        | null -> Error (sprintf "Type not found: %s" typeName)
        | t ->
            Symbols.addGlobal typeName (Type t)
            Ok (Bool true)
    | [_] -> Error "First arg must be a string"
    | _ -> Error "Wrong number of arguments"

let defMacro (eval : Form -> Result<Form, string>) (args : Form list) =
    match args with
    | Symbol name :: Vector args' :: body ->
        Symbols.addGlobal name (Macro (args', body))
        Ok (Bool true)
    | _ ->
        Error "Wrong arguments"

let specialForms =
    Map.ofList [
        (".",           (SpecialForm invoke))
        ("def",         (SpecialForm def))
        ("deftype",     (SpecialForm defType))
        ("defmacro",    (SpecialForm defMacro))
    ]

let tryFind name = specialForms |> Map.tryFind name
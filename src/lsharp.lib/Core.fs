module LSharp.Core

open System.Collections.Immutable

type Number = int
//| Int32 of int

type SpecialForm =
| Dot
| Def
| DefMacro
| Fn
| If
| Do
| Let
| Quote

//type Fn = {
//    Invoke : Form list -> Result<Form, string>
//}
type Form = 
| Empty
| Nil
| Bool of bool
| String of string
| Symbol of string
| Keyword of string
| Number of Number
| Quote of Form
| List of Form list
| Vector of Form array
| Map of ImmutableDictionary<Form, Form>
| Set of ImmutableHashSet<Form>
| Object of obj
| Type of System.Type
| Fn of ((Form -> Result<Form,string>) -> Form list -> Result<Form, string>)

type EvalBuilder() =
    member this.Bind(m, f) = Result.bind f m
        //match m with
        //| Ok res -> f res
        //| Error err -> Error err

    member this.Return(x) = Ok x
    member this.ReturnFrom(x) = x
    
let evaluate = new EvalBuilder()

// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open lsharp.test

// Define your library scripting code here


type MaybeBuilder() =
    member this.Bind(x, f) = Option.bind f x
        //match x with
        //| None -> None
        //| Some a -> f a

    member this.Return(x) =
        Some x
       
let maybe = new MaybeBuilder()

let divideBy bottom top =
    match bottom with
    | 0 -> None
    | _ -> Some (top / bottom)


let divideByWorkflow init x y z =
    maybe {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
    }

divideByWorkflow 12 3 2 1
divideByWorkflow 12 0 2 1

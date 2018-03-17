module LSharp.Symbols

open LSharp.Core

module Internal =
    let mutable globalSymbols = Map.empty<string, Form>

    let combine a b = 
        match a with
        | Some _ -> a  // a succeeds -- use it
        | None -> b    // a fails -- use b instead

    // create an infix version
    let ( <++ ) = combine

open Internal

let addGlobal name f =
    Internal.globalSymbols <- Map.add name f Internal.globalSymbols

let tryFindGlobal name = Internal.globalSymbols |> Map.tryFind name

let tryFind sym1 name = 
    (sym1 name)
    <++ (tryFindGlobal name)
module LSharp.Symbols

open LSharp.Core
open LSharp.SpecialForms

module Internal =
    let specialForms = Map.ofList [ (".", (Fn invokeDot)) ]
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

let tryFindSpecial name = Internal.specialForms |> Map.tryFind name
let tryFindGlobal name = Internal.globalSymbols |> Map.tryFind name

let tryFind name =
    (Internal.specialForms |> Map.tryFind name)
    <++ (Internal.globalSymbols |> Map.tryFind name)
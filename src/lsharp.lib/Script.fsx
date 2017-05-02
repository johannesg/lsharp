﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "../../packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsecCS.dll"
#r "../../packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsec.dll"
#load "Library1.fs"
open lsharp.lib
open FParsec

// Define your library scripting code here

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printf "Failure: %s" errorMsg

let str s : Parser<_> = pstring s

let ptrue : Parser<_> = pstring "true"


test symbol "++ -"
test symbol "_+a+sdf"



test pbool "false"

test form "++"
test form "true false"
test form "+1e"
test form "1"

test form ":apa"

test form "(:a :a)"
test form "( :a (:b :c))"
test form "(:a )"
test form "( :a)"
test form "()"
test form "( )"

test form "[:a :a]"
test form "( :a [:b :c])"
test form "[:a ]"
test form "[ :a]"
test form "[]"
test form "[ ]"

test form "{ :a :b }"
test form "{ :a,:b }"
test form "{ :a ,:b }"
test form "{ :a, :b :c }"

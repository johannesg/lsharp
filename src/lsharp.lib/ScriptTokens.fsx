// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
// #r "../../packages/FParsec/lib/portable-net45+win8+wp8+wpa81/FParsecCS.dll"
// #r "../../packages/FParsec/lib/portable-net45+win8+wp8+wpa81/FParsec.dll"
#load "Tokenizer.fs"
open LSharp.Core.Tokenizer
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

test token "++"
test token "1234"
test token "01234"
test token "0x1234"
test tokenList "true false "
test tokenList "+1e2"
test tokenList "1"

test tokenList ":apa"

test tokenList "( :a :a )"
test tokenList "(:a :a)"
test tokenList "(:a ) "
test tokenList "( :a (:b :c))"
test tokenList "(:a )"
test tokenList "( :a)"
test tokenList "()"
test tokenList "( )"

test tokenList "[:a :a]"
test tokenList "( :a [:b :c])"
test tokenList "[:a ]"
test tokenList "[ :a]"
test tokenList "[]"
test tokenList "[ ]"

test tokenList "{ :a :b }"
test tokenList "{ :a,:b }"
test tokenList "{ :a ,:b }"
test tokenList "{ :a, :b :c }"

test tokenList " :a  "

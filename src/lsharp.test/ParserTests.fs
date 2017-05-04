module LSharp.Test.ParserTests

open Fuchu
open FParsec
open LSharp.Core.Parser

let test p str =
    let name = sprintf "Parsing: '%s'" str

    testCase name <| fun _ ->
        match run p str with
        | Success(result, _, _) -> 
            ()
            //printf "Success: %A\n" result
        | Failure(errorMsg, _, _) -> 
            //printf "Failure: %s" errorMsg |> ignore
            failtest errorMsg
        //Assert.Raise(errorMsg, null, (fun x -> ()))

let testSymbol p str =
    let name = sprintf "Parsing: '%s'" str

    testCase name <| fun _ ->
        match run p str with
        | Success(result, _, _) -> 
            Assert.Equal("Should parse everything", str, result)
            //printf "Success: %A\n" result
        | Failure(errorMsg, _, _) -> 
            //printf "Failure: %s" errorMsg |> ignore
            failtest errorMsg
        //Assert.Raise(errorMsg, null, (fun x -> ()))

let testFail p str =
    let name = sprintf "Parsing: '%s'" str

    testCase name <| fun _ ->
        match run p str with
        | Success(result, _, _) -> 
            failtest (sprintf "Should fail: %A" result)
            //printf "Success: %A\n" result
        | Failure(errorMsg, _, _) -> 
            //printf "Failure: %s" errorMsg |> ignore
            failtest errorMsg
        //Assert.Raise(errorMsg, null, (fun x -> ()))

[<Tests>]
let parseTests = 
    testList "Various parses" [
        test symbol "++ -"
        test symbol "_+a+sdf"
        test symbol "_öäå"

        test symbol "/"
        test symbol "ns1/bb"
        testFail symbol "/bb"
        testFail symbol "ns1//bb"
        testFail symbol "ns1/bb/aa"
        testFail symbol "ns1/bb/"

        test symbol "ns1$bb"
        test symbol "ns1%bb"
        test symbol "+"

        test pbool "false"

        test form "++"
        test form "true false"
        test form "+e"
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
        test form "{ :a, :b }"

        test parser "   :a  "
    ]


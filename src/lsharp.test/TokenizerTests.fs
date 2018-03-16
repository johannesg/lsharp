module LSharp.Test.TokenizerTests

open Expecto
open FParsec
open LSharp.Tokenizer

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
            Expect.equal str result "Should parse everything"
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
    testList "Various tokens" [
        test tokenList "++ -"
        test symbol "_+a+sdf"
        test symbol "_öäå"

        test symbol "/"
        test symbol "ns1/bb"
        //testFail symbol "/bb"
        //testFail symbol "ns1//bb"
        //testFail symbol "ns1/bb/aa"
        //testFail symbol "ns1/bb/"

        test symbol "ns1$bb"
        test symbol "ns1%bb"
        test symbol "+"

        test tokenList "12.34"

        test pbool "false"

        test tokenList "++"
        test tokenList "true false"
        test tokenList "+e"
        test tokenList "1"

        test tokenList ":apa"

        test tokenList "(:a :a)"
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
        test tokenList "{ :a, :b }"

        test tokenList "   :a  "
    ]


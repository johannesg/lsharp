module LSharp.Test.ReaderTests

open Expecto
open LSharp.Core.Reader

let test str expected expectRest =
    let name = sprintf "Parsing: '%s'" str

    testCase name <| fun _ ->
        match read str with
        | Ok(result, rest) -> 
            //printf "Success: %A\n" result
            Expect.equal result expected ""
            Expect.equal rest expectRest ""
            ()
        | Error(errorMsg) -> 
            //printf "Failure: %s" errorMsg |> ignore
            failtest errorMsg
        //Assert.Raise(errorMsg, null, (fun x -> ()))

[<Tests>]
let parseTests = 
    testList "Various parses" [
        test "abc" (Symbol "abc") ""
        test "_abc" (Symbol "_abc") ""
        test "_abcd" (Symbol "_abcd") ""
        test "  _abcd" (Symbol "_abcd") ""
        
    ]

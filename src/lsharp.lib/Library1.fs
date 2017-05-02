module lsharp.parser

open FParsec
open FParsec.Primitives
open System.Text
open Microsoft.FSharp.Collections

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

type Symbol = string

type Number =
| Int32 of int
| Int64 of System.Int64
| Float of double
| Ratio of int * int

type Literal =
| Boolean of bool
| String of string
| Number of Number
| Nil
| Keyword of string

type Form =
| Symbol of Symbol
| Literal of Literal
| List of Form list
| Vector of Form[]
| Map of Map<Form,Form>

let ch2str cl =
    string (List.fold
              (fun (sb:StringBuilder) (c:char) -> sb.Append(c))
              (new StringBuilder())
              cl)

// let ch : Parser<_> = choice [pchar 'a' ; pchar 'b']

let pname : Parser<_> =
    let special = anyOf "*+!-_\'"
    let first = choice [special; letter]
    let foll = choice [special; letter; digit]

    first .>>. many foll
    |>> (fun (a,b) -> ch2str (a :: b))

let symbol : Parser<_> =
    pname |>> (fun x -> Symbol(x))

let keyword : Parser<_> =
    pchar ':' >>. pname |>> (fun x -> Keyword(x))
    
let pbool : Parser<_> =
    let ptrue = pstring "true" >>% true
    let pfalse = pstring "false" >>% false
    choice [ptrue; pfalse] |>> (fun x -> Boolean(x))

// http://www.quanttec.com/fparsec/reference/charparsers.html#members.numberLiteral

// We want to support decimal or hexadecimal numbers with an optional minus
// sign. Integers may have an 'L' suffix to indicate that the number should
// be parsed as a 64-bit integer.
let numberFormat = NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowPlusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent
                   ||| NumberLiteralOptions.AllowHexadecimal
                   ||| NumberLiteralOptions.AllowSuffix

let pnumber : Parser<_> =
    let parser = numberLiteral numberFormat "number"
    fun stream ->
        let reply = parser stream
        if reply.Status = Ok then
            let nl = reply.Result // the parsed NumberLiteral
            if nl.SuffixLength = 0
               || (   nl.IsInteger
                   && nl.SuffixLength = 1 && nl.SuffixChar1 = 'L')
            then
                try
                    let result = if nl.IsInteger then
                                     if nl.SuffixLength = 0 then
                                         Int32 (int32 nl.String)
                                     else
                                         Int64 (int64 nl.String)
                                 else
                                     if nl.IsHexadecimal then
                                         Float (floatOfHexString nl.String)
                                     else
                                         Float (float nl.String)
                    Reply(result)
                with
                | :? System.OverflowException as e ->
                    stream.Skip(-nl.String.Length)
                    Reply(FatalError, messageError e.Message)
            else
                stream.Skip(-nl.SuffixLength)
                Reply(Error, messageError "invalid number suffix")
        else // reconstruct error reply
            Reply(reply.Status, reply.Error)

let pnumber2 :Parser<_> = pnumber |>> (fun x -> Number(x))

let nil : Parser<_> = pstring "nil" >>% Nil

let literal : Parser<_> = choice [nil;pnumber2;pbool;keyword] |>> (fun x -> Literal(x))

let form : Parser<_> =
    let form, formImpl = createParserForwardedToRef<Form, unit>()

    let formSeq = spaces >>. (sepEndBy form spaces1)

    let plist =
        pchar '(' >>. formSeq .>> (pchar ')')
        |>> (fun x -> List(x))

    let pvec =
        pchar '[' >>. formSeq .>> (pchar ']')
        |>> (fun x -> Vector(Array.ofList x))

    let pmap =
        let ws = (attempt (tuple3 spaces (pchar ',') spaces |>> ignore)) <|> spaces1

        let keyvalue =
            pipe3 literal ws literal (fun k _ v -> (k,v))

        let kvseq = spaces >>. (sepEndBy keyvalue spaces1)

        pchar '{' >>. kvseq .>> (pchar '}')
        |>> (fun x -> Map( Map.ofList x))

    do formImpl := choice [literal;symbol;plist;pvec;pmap]
    form


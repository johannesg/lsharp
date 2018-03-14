module LSharp.Core.Parser

open FParsec
open FParsec.Primitives
open System.Text
open Microsoft.FSharp.Collections
open System.Diagnostics

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

let ws : Parser<_> = spaces
let ws1 : Parser<_> = spaces1

// let pname label : Parser<_> =
    // |>> (fun (a,b) -> ch2str (a :: b))

let symbol : Parser<_> =
    let isSpecial = isAnyOf "*+!-_\'./"
    let firstChar c = isSpecial c || isLetter c
    let nextChar c = isSpecial c || isLetter c || isDigit c

    many1Satisfy2L firstChar nextChar "symbol"
    |>> (fun x -> Symbol(x))

let keyword : Parser<_> =
    let isSpecial = isAnyOf "*+!-_\'."
    let firstChar c = c = ':'
    let nextChar c = isSpecial c || isLetter c || isDigit c

    many1Satisfy2L firstChar nextChar "keyword"
    |>> (fun x -> Keyword(x))
    
let pbool : Parser<_> =
    let ptrue = stringReturn "true" (Boolean(true))
    let pfalse = stringReturn "false" (Boolean(false))
    ptrue <|> pfalse

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

let nil : Parser<_> = stringReturn "nil" Nil

let literal : Parser<_> = choice [nil;pnumber2;pbool;keyword] |>> (fun x -> Literal(x))

let form : Parser<_> =
    let form, formImpl = createParserForwardedToRef<Form, unit>()

    let formSeq = spaces >>. (sepEndBy form spaces)

    let betweench cb ce p = between (pchar cb) (pchar ce) p

    let plist =
        betweench '(' ')' formSeq
        |>> (fun x -> List(x))

    let pvec =
        betweench '[' ']' formSeq
        |>> (fun x -> Vector(Array.ofList x))

    let pmap =
        let ws = (attempt (tuple3 spaces (pchar ',') spaces |>> ignore)) <|> spaces1

        let keyvalue =
            pipe3 literal ws literal (fun k _ v -> (k,v))

        let kvseq = spaces >>. (sepEndBy keyvalue spaces1)

        betweench '{' '}' kvseq 
        |>> (fun x -> Map( Map.ofList x))

    do formImpl := choice [literal;symbol;plist;pvec;pmap]
    //do formImpl := plist
    form

let parser : Parser<_> =
    ws >>. form .>> (ws .>> eof)

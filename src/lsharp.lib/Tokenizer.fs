module LSharp.Tokenizer

open FParsec

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

type Token =
| Literal of Literal
| Symbol of Symbol
| OpenParen
| CloseParen
| OpenBracket
| CloseBracket
| OpenBrace
| CloseBrace

let ws : Parser<_> = skipManySatisfy (fun c ->
    c = '\n' || c = '\t' || c = ' ' || c = ',')

// let pname label : Parser<_> =
    // |>> (fun (a,b) -> ch2str (a :: b))

let symbol : Parser<_> =
    let isSpecial = isAnyOf "*+!-_\'./"
    let firstChar c = isSpecial c || isLetter c
    let nextChar c = isSpecial c || isLetter c || isDigit c

    many1Satisfy2L firstChar nextChar "symbol"
    |>> Symbol

let keyword : Parser<_> =
    let isSpecial = isAnyOf "*+!-_\'."
    let firstChar c = c = ':'
    let nextChar c = isSpecial c || isLetter c || isDigit c

    many1Satisfy2L firstChar nextChar "keyword"
    |>> Keyword
    
let pbool : Parser<_> =
    stringReturn "true" true
    <|> stringReturn "false" false
    |>> Boolean

let intPat ="([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?"
let rationPat = "([-+]?[0-9]+)/([0-9]+)"
let floatPat = "([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?"

let charToInt ( c : char) = function
| '0' -> 0  
| '1' -> 1
| '2' -> 2  
| '3' -> 3  
| '4' -> 4  
| '5' -> 5  
| '6' -> 6  
| '7' -> 7  
| '8' -> 8  
| '9' -> 9  
| 'a' | 'A' -> 10
| 'b' | 'A' -> 10
| 'c' | 'A' -> 10
| 'd' | 'A' -> 10
| 'e' | 'A' -> 10
| 'f' | 'A' -> 10

let pnumber2 : Parser<_> =
    let isNegative = choice [ charReturn '-' true; charReturn '+' false; lookAhead digit >>% false ]
    let poctal = pchar '0' >>. (many1 octal) |>> (fun x -> (x, 8))
    let phex = pchar '0' >>. anyOf "xX" >>. (many1 hex) |>> (fun x -> (x, 16)) 
    let pdec = anyOf "123456789" .>>. (many1 digit) |>> (fun (a, b) -> (a :: b, 10))
    pdec 
    |>> (fun (n, r) -> Number(Int32(System.Int32.Parse(System.String(Seq.toArray(n))))))

let pnumber : Parser<_> =
    choice [
        pint32 |>> Int32
        pint64 |>> Int64
        pfloat |>> Float
    ] |>> Number

let nil : Parser<_> = stringReturn "nil" Nil

let literal : Parser<_> = choice [nil;pnumber2;pbool;keyword] |>> Literal

let token : Parser<_> =
    choice [
        charReturn '(' OpenBrace
        charReturn ')' CloseBrace
        charReturn '[' OpenBracket
        charReturn ']' CloseBracket
        charReturn '{' OpenBrace
        charReturn '}' CloseBrace
        literal;symbol
        ]

let tokenList : Parser<_> =
    ws >>. (many (token .>> ws)) .>> eof

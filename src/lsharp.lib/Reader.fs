module LSharp.Reader

open LSharp.Core

open System
open System.Collections.Immutable

module Internal =
    type ParserBuilder() =
        member this.Bind(m, f) = Result.bind f m
        member this.Return(x) = Ok x
        member this.ReturnFrom(x) = x

    let parse = new ParserBuilder()

    let (|AnyOf|_|) (chars : string) (c : char) =
        if chars.IndexOf(c) > -1 then Some() else None

    let (|IsDigit|_|) (c : char) =
        if Char.IsDigit c then Some() else None

    let isTerminating (c : char) =
        "\";\\@^`~()[]{}".IndexOf(c) > -1

    let isWhitespace (c : char) =
        Char.IsWhiteSpace c ||c = ','

    let toOption (b : bool) =
        if b then Some() else None

    let (|IsWhiteSpace|_|) (c : char) =
        toOption (isWhitespace c)

    let (|IsTerminating|_|) (c : char) =
        isTerminating >> toOption

    let toStr (cl : char list) =
        System.String(List.toArray cl)

    let isTerminatingOrWhitespace c =
        isTerminating c || isWhitespace c

    let readNumber (cl : char list) =
        let ncl = List.takeWhile (not << isTerminatingOrWhitespace) cl
        let rest = List.skipWhile (not << isTerminatingOrWhitespace) cl
        let ns = toStr ncl
        match Int32.TryParse(ns) with
        | (true, n) -> Ok(Number(n), rest)
        | _ -> Error(sprintf "Failed to parse number: %s" ns)

    let readToken (cl : char list) =
        let ncl = List.takeWhile (not << isTerminatingOrWhitespace) cl
        let rest = List.skipWhile (not << isTerminatingOrWhitespace) cl

        match ncl with
        | ['/'] -> Ok(Symbol "/", rest)
        | '/' :: _ -> Error("Invalid symbol")
        | ['.'] -> Ok (Symbol ".", rest)
        | ':' :: r -> Ok(Keyword (toStr r), rest)
        | _ ->
            let str = toStr ncl
            match str with
            | "true" -> Ok (Bool( true), rest)
            | "false" -> Ok (Bool( false), rest)
            | "nil" -> Ok (Nil, rest)
            | _ -> Ok(Symbol( str), rest)

    let readList read2 (cl : char list) =
        let rec readR l (cl : char list) =
            let cl = List.skipWhile isWhitespace cl
            match cl with
            | [] -> Error("List has no end")
            | ')' :: rest -> Ok (List(List.rev l), rest)
            | _ ->
                parse {
                    let! (v, rest) = read2 cl
                    return! readR (v :: l) rest
                }
        
        readR [] cl

    let readArray read2 (cl : char list) =
        let rec readR l (cl : char list) =
            let cl = List.skipWhile isWhitespace cl
            match cl with
            | [] -> Error("Array has no end")
            | ']' :: rest -> Ok (Vector(l |> List.rev |> List.toArray), rest)
            | _ ->
                parse {
                    let! (v, rest) = read2 cl
                    return! readR (v :: l) rest
                }
        
        readR [] cl

    let readMap read2 (cl : char list) =
        let rec readR m (cl : char list) =
            let cl = List.skipWhile isWhitespace cl
            match cl with
            | [] -> Error("List has no end")
            | '}' :: rest -> Ok (Map(m), rest)
            | _ ->
                parse {
                    let! (k, cl') = read2 cl
                    let! (v, rest) = read2 cl'
                    return! readR (m.Add (k, v)) rest
                }
        
        readR ImmutableDictionary.Empty cl

    let readSet read2 (cl : char list) =
        let rec readR s (cl : char list) =
            let cl = List.skipWhile isWhitespace cl
            match cl with
            | [] -> Error("List has no end")
            | '}' :: rest -> Ok (Set s, rest)
            | _ ->
                parse {
                    let! (v, rest) = read2 cl
                    return! readR (s.Add (v)) rest
                }
        
        readR ImmutableHashSet.Empty cl

    let readQuote read2 (cl : char list) =
        match read2 cl with
        | Ok (f, rest) -> Ok (Quote f, rest)
        | err -> err

    let readString (cl : char list) =
        let rec readR str cl =
            match cl with
            | [] -> Error("No end of string")
            | '"' :: rest -> Ok (Form.String(str |> List.rev |> toStr), rest)
            | c :: rest -> readR (c :: str) rest

        readR [] cl


    let rec read2 (cl : char list) =
        let cl = List.skipWhile isWhitespace cl
        match cl with
        | [] -> Ok (Empty, [])
        //| IsWhiteSpace :: rest -> read2 rest
        | AnyOf "])}" :: _ -> Error(sprintf "Unmatched container") 
        | '\'' :: rest -> readQuote read2 rest
        | '"' :: rest -> readString rest
        | IsDigit :: _ -> readNumber cl
        | AnyOf "-+" :: IsDigit :: _ -> readNumber cl
        | '(' :: rest -> readList read2 rest
        | '[' :: rest -> readArray read2 rest
        | '{' :: rest -> readMap read2 rest
        | '#' :: '{' :: rest -> readSet read2 rest
        //| ch :: rest -> Ok (End, rest)
        | _ -> readToken cl

open Internal
let read (s : string) = 
    let cl = Seq.toList s

    let res = read2 cl
    Result.map (fun (f, cl) -> (f, toStr cl)) res

let readAll (s : string) =
    let cl = Seq.toList s
    let rec readR fl cl =
        let res = read2 cl
        match res with
        | Ok (Empty, []) -> Ok (List.rev fl)
        | Ok (f, rest) -> readR (f :: fl) rest
        | Error(err) -> Error(err)

    readR [] cl
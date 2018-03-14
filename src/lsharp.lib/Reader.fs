module LSharp.Core.Reader

open LSharp.Core

open System
open System.Collections.Immutable

//type Char =
//| Char of char * string
//| Whitespace of string
//| Eof

//let getCh (s : string) =
//    if s.Length = 0 then
//        Eof
//    else
//        let ch = s.Chars 0
//        if System.Char.IsWhiteSpace(ch) then
//            Whitespace (s.Substring(1))
//        else
//            Char (s.Chars 0, s.Substring(1))

type MaybeBuilder() =
    member this.Bind(m, f) = Result.bind
    member this.Return(x) = Ok x

let maybe = new MaybeBuilder()

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
    | ':' :: r -> Ok(Keyword (toStr r), rest)
    | _ -> Ok(Symbol (toStr ncl), rest)

let readList read2 (cl : char list) =
    let rec readR l (cl : char list) =
        let cl = List.skipWhile isWhitespace cl
        match cl with
        | [] -> Error("List has no end")
        | ')' :: rest -> Ok (List(List.rev l), rest)
        | _ ->
            match read2 cl with
            | Ok (v, rest) -> readR (v :: l) rest
            | err -> err
    
    readR [] cl

let readArray read2 (cl : char list) =
    let rec readR l (cl : char list) =
        let cl = List.skipWhile isWhitespace cl
        match cl with
        | [] -> Error("Array has no end")
        | ']' :: rest -> Ok (Vector(l |> List.rev |> List.toArray), rest)
        | _ ->
            match read2 cl with
            | Ok (v, rest) -> readR (v :: l) rest
            | err -> err
    
    readR [] cl

let pipe2 r1 r2 cl =
    match r1 cl with
    | Ok (t1, rest) -> 
        match r2 rest with
        | Ok(t2, rest2) -> Ok ((t1, t2), rest2)
        | Error(err) -> Error (err)
    | Error(err) -> Error (err)

let readMap read2 (cl : char list) =
    let rec readR m (cl : char list) =
        let cl = List.skipWhile isWhitespace cl
        match cl with
        | [] -> Error("List has no end")
        | '}' :: rest -> Ok (Map(m), rest)
        | _ ->
            let res = pipe2 read2 read2 cl

            match res with
            | Ok ((k, v), rest) -> readR (m.Add( k, v)) rest
            | Error(err) -> Error(err)
    
    readR ImmutableDictionary.Empty cl

let readSet read2 (cl : char list) =
    let rec readR s (cl : char list) =
        let cl = List.skipWhile isWhitespace cl
        match cl with
        | [] -> Error("List has no end")
        | '}' :: rest -> Ok (Set s, rest)
        | _ ->
            match read2 cl with
            | Ok (v, rest) -> readR (s.Add(v)) rest
            | err -> err
    
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




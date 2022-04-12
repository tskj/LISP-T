module Combinators 

open System

type ParseResult<'token, 'a>
    = Success of 'a * ('token list)
    | Fail of string list

type Parser<'token, 'a> =
    'token list -> ParseResult<'token, 'a>

let pBind f p =
    fun input ->
        match p input with
        | Fail e -> Fail e
        | Success (parsed, rest) ->
            f parsed rest

let pReturn x =
    fun input -> Success (x, input)
        
let pBindFail f p =
    fun input ->
        match p input with
        | Success (parsed, input) -> Success (parsed, input)
        | Fail e ->
            f e input

let pReturnFail e =
    fun _input -> Fail e

type ParserBuilder() =
    member this.Bind(p: Parser<'token, 'a>, f: 'a -> Parser<'token, 'b>) =
        pBind f p
    member this.Return(x): Parser<'token, 'a> =
        pReturn x
    member this.ReturnFrom(x): Parser<'token, 'a> =
        x
let parser = ParserBuilder()

let pMap f p =
    parser {
        let! parsed = p
        return f parsed
    }

let withError errorMsgs parser =
    parser |> (pBindFail
        (fun msgs -> pReturnFail <| msgs @ errorMsgs))

let replaceError errorMsgs parser =
    parser |> (pBindFail
        (fun _msgs -> pReturnFail <| errorMsgs))

let recognize (p: 'token -> bool): Parser<'token, 'token> =
    function
    | [] -> Fail [$"Expected token, but reached end of input"]
    | input :: rest ->
        if p input then
            Success (input, rest)
        else
            Fail [$"Couldn't find token in {input :: rest}"]

let accept token: Parser<'token, 'token> =
    recognize ((=) token)
    |> withError [$"Expected {token}"]

let drop p: Parser<'token, unit> =
    parser {
        let! _ = p
        return ()
    }

let orElse (p1: Parser<'token, 'a>) (p2: Parser<'token, 'a>) =
    p1 |> (pBindFail
        (fun e1 ->
            p2 |> (pBindFail
                (fun e2 -> pReturnFail <| e1 @ e2))))

let andThen (p1: Parser<'token, 'a list>) (p2: Parser<'token, 'a>) =
    parser {
        let! r1 = p1
        let! r2 = p2
        return r1 @ [r2]
    }

let choose (parsers: Parser<'token, 'a> list): Parser<'token, 'a> =
    parsers
    |> List.fold orElse (pReturnFail [])

let chain (parsers: Parser<'token, 'a> list): Parser<'token, 'a list> =
    parsers
    |> List.fold andThen (pReturn [])

let rec many p =
    p |> repeat
    |> pBindFail (fun _msgs -> pReturn [])

and repeat (p: Parser<'token, 'a>): Parser<'token, 'a list> =
    parser {
        let! parsed = p
        let! restParsed = p |> many
        return parsed :: restParsed
    }

let option (p: Parser<'token, 'a>): Parser<'token, 'a list> =
    parser {
        let! parsed = p
        return [parsed]
    }
    |> pBindFail (fun _msgs -> pReturn [])


let parses (s: string): Parser<char,string> =
    chain (s |> Seq.toList |> List.map accept)
    |> pMap (fun s -> String.Join("", s))

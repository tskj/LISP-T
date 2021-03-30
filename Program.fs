
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
let parser = ParserBuilder()

let pMap f p =
    parser {
        let! parsed = p
        return f parsed
    }

type LexerBuilder() =
    member this.Yield(p) =
        p |> pMap (fun x -> [x])
    member this.YieldFrom(p) =
        p 
    member this.Combine(p1, p2) =
        parser {
            let! x = p1
            let! y = p2
            return x @ y
        }
    member this.Zero() =
        pReturn []
    member this.Delay(f) =
        f()
let lexer = LexerBuilder()

let withError errorMsgs parser =
    parser|> (pBindFail
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

let drop token: Parser<'token, unit> =
    parser {
        let! _ = accept token
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


[<EntryPoint>]
let main argv =
    let input =
        "ABCCBADBA"
        |> Seq.toList
        
    printfn "%A"
        <| choose [parses "BC"; parses "ABC"] input
        
    printfn "%A"
        <| chain [accept 'A'; accept 'B'] input
        
    printfn "%A"
        <| repeat (choose [accept 'A'; accept 'B'; accept 'C']) input
        
    printfn "%A"
        <| many (choose [accept 'A'; accept 'C']) input
        
    printfn "%A"
        <| option (choose [accept 'B'; accept 'C']) input

    input
    |> parser {
        let! x = accept 'A'
        let! y = accept 'C' |> option
        let! z = choose [accept 'B'; accept 'C'] |> repeat
        return x :: y @ z
    }
    |> printfn "%A"
    
    input
    |> lexer {
        yield accept 'A'
        yield! accept 'C' |> option
        yield! choose [accept 'B'; accept 'C'] |> repeat
        yield accept 'A'
    }
    |> printfn "%A"
    
    0 


open System

type ParseResult<'token, 'a>
    = Success of 'a * ('token list)
    | Fail of string list

type Parser<'token, 'a> =
    'token list -> ParseResult<'token, 'a>
            
let pMap f parser input =
    match parser input with
    | Fail e -> Fail e
    | Success (p, input) -> Success (f p, input)

let pBind parser f input =
    match parser input with
    | Fail e -> Fail e
    | Success (parsed, rest) ->
        f parsed rest

let pReturn x input =
    Success (x, input)

let withError errorMsgs parser input =
    match parser input with
    | Success (parsed, rest) -> Success (parsed, rest)
    | Fail msgs -> Fail <| msgs @ errorMsgs

type ParserBuilder() =
    member this.Bind(p: Parser<'token, 'a>, f: 'a -> Parser<'token, 'b>) =
        pBind p f
    member this.Return(x): Parser<'token, 'a> =
        pReturn x
let parser = ParserBuilder()

let recognize (p: 'token -> bool): Parser<'token, 'token> =
    function
    | [] -> Fail [$"Expected token, but reached end of input"]
    | input :: rest ->
        if p input then
            Success (input, rest)
        else
            Fail [$"Found {input} in {input :: rest}"]

let accept token: Parser<'token, 'token> =
    recognize ((=) token)
    |> withError [$"Expected {token}"]

let drop token: Parser<'token, unit> =
    accept token |> pMap (fun _ -> ())

let orElse (p1: Parser<'token, 'a>) (p2: Parser<'token, 'a>) input =
    match p1 input with
    | Success (a,b) -> Success (a,b)
    | Fail e1 ->
        match p2 input with
        | Success (a,b) -> Success (a,b)
        | Fail e2 ->
            Fail <| e1 @ e2

let andThen (p1: Parser<'token, 'a list>) (p2: Parser<'token, 'a>) =
    parser {
        let! r1 = p1
        let! r2 = p2
        return r1 @ [r2]
    }

let choose (parsers: Parser<'token, 'a> list): Parser<'token, 'a> =
    parsers
    |> List.fold orElse (fun _ -> Fail [])

let chain (parsers: Parser<'token, 'a> list): Parser<'token, 'a list> =
    parsers
    |> List.fold andThen (pReturn [])

let rec many parser input =
    match parser input with
    | Fail _ -> Success ([], input)
    | Success (parsed, rest) ->
        let next = many parser rest
        match next with
        // This branch can`t happen
        | Fail _ -> Success ([parsed], rest)
        | Success (restParsed, rest') ->
            Success (parsed :: restParsed, rest')
            
let repeat (p: Parser<'token, 'a>): Parser<'token, 'a list> =
    parser {
        let! parsed = p
        let! restParsed = p |> many
        return parsed :: restParsed
    }

let option parser input =
    match parser input with
    | Fail _ -> Success ([], input)
    | Success (parsed, rest) -> Success ([parsed], rest)

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

    let res =
        input
        |> parser {
            let! x = accept 'A'
            let! y = accept 'C' |> option
            let! z = choose [accept 'B'; accept 'C'] |> repeat
            return x :: y @ z
        }

    printfn "%A" res
        
    0 

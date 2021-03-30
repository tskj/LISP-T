
open System

type ParseResult<'token, 'a>
    = Success of 'a * ('token list)
    | Fail of string list

type Parser<'token, 'a> =
    'token list -> ParseResult<'token, 'a>

let accept (c: 'token): Parser<'token, 'token> =
    function
    | [] -> Fail [$"Expected token {c}, but reached end of input"]
    | input :: rest ->
        if c = input then
            Success (input, rest)
        else
            Fail [$"Expected token {c}, but found {input} in {input :: rest}"]
            
let pMap f =
    function
    | Success (p, input) -> Success (f p, input)
    | Fail e -> Fail e

let drop (c: 'token) input: ParseResult<'token, unit> =
    accept c input |> pMap (fun _ -> ())

let withError errorMsgs =
    function
    | Success (a,b) -> Success (a,b)
    | Fail msgs -> Fail <| msgs @ errorMsgs
       
let orElse (p1: Parser<'token, 'a>) (p2: Parser<'token, 'a>) input =
    let r1 = p1 input
    let r2 = p2 input
    match r1 with
    | Success (a,b) -> Success (a,b)
    | Fail _ ->
        match r2 with
        | Success (a,b) -> Success (a,b)
        | Fail e2 ->
            r1 |> withError e2
            
let andThen (p1: Parser<'token, 'a list>) (p2: Parser<'token, 'a>) input =
    match p1 input with
    | Fail e -> Fail e
    | Success (x, rest) ->
        match p2 rest with
        | Fail e -> Fail e
        | Success (y, rest') ->
            Success (x @ [y], rest')

let choose (parsers: Parser<'token, 'a> list): Parser<'token, 'a> =
    parsers
    |> List.fold orElse (fun _ -> Fail [])

let chain (parsers: Parser<'token, 'a> list): Parser<'token, 'a list> =
    parsers
    |> List.fold andThen (fun input -> Success ([], input))
    
let rec any parser input =
    match parser input with
    | Fail _ -> Success ([], input)
    | Success (parsed, rest) ->
        let next = any parser rest
        match next with
        // This branch can`t happen
        | Fail _ -> Success ([parsed], rest)
        | Success (restParsed, rest') ->
            Success (parsed :: restParsed, rest')
        
let repeat parser input =
    match parser input with
    | Fail e -> Fail e
    | Success (parsed, rest) ->
        let next = any parser rest
        match next with
        // This branch can't happen
        | Fail _ -> Success ([parsed], rest)
        | Success (restParsed, rest') ->
            Success (parsed :: restParsed, rest')
            
let option parser input =
    match parser input with
    | Fail _ -> Success (None, input)
    | Success (parsed, rest) -> Success (Some parsed, rest)

let parses (s: string): Parser<char,char list> =
    chain (s |> Seq.toList |> List.map accept)

type ParserBuilder() =
    
    member this.Bind(p: Parser<'token, 'a>, f: 'a -> Parser<'token, 'b>) =
        fun input ->
            match p input with
            | Fail e -> Fail e
            | Success (parsed, rest) ->
                f parsed rest

    member this.Return(x): Parser<'token, 'a> =
        fun input -> Success (x, input)

let parser = ParserBuilder()

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
        <| any (choose [accept 'A'; accept 'C']) input
        
    printfn "%A"
        <| option (choose [accept 'B'; accept 'C']) input
        
    let res =
        input
        |> parser {
            let! x = accept 'A'
            let! y = accept 'C' |> option
            let! z = choose [accept 'B'; accept 'C'] |> repeat
            return x :: z
        }
        
    printfn "%A" res
        
    0 

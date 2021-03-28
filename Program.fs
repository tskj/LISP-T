
open System

type ParseResult<'tokenStream, 'a>
    = Success of 'a * 'tokenStream
    | Fail of string list

type Parser<'tokenStream, 'a> =
    'tokenStream -> ParseResult<'tokenStream, 'a>

let parsec (c: char): Parser<char list, char> =
    function
    | [] -> Fail [$"Expected character {c}, but reached end of iput"]
    | input :: rest ->
        if c = input then
            Success (input, rest)
        else
            Fail [$"Expected character {c}, but found {input} in {input :: rest}"]
    
let parses (s: string): Parser<char list, char list>=
    fun input ->
        s
        |> Seq.toList
        |> List.map parsec
        |> (Success ([], input)
            |> List.fold
                   (fun acc p ->
                    match acc with
                    | Fail f -> Fail f
                    | Success (so_far, input) ->
                        let res = p input
                        match res with
                        | Fail f -> Fail f
                        | Success (next, input) ->
                            Success (so_far @ [next], input)))
        
let withError errorMsgs =
    function
    | Success (a,b) -> Success (a,b)
    | Fail msgs -> Fail <| msgs @ errorMsgs
       
let choose parserList input =
    parserList
    |> (Fail []
    |> List.fold
           (fun acc p ->
                match acc with
                | Success (a,b) -> Success (a,b)
                | Fail error ->
                    p input
                    |> withError error))

[<EntryPoint>]
let main argv =
    let input =
        "ABC"
        |> Seq.toList
    let res = choose [parses "BC"; parses "xx"] input
    printf "%A" res
    0 

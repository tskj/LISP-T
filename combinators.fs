module rec FSharpParserCombinator

open System

module Parser =

  type ParseResult<'token, 'a>
    = Success of 'a * ('token seq)
    | Fail of string list

  type Parser<'token, 'a> =
    'token seq -> ParseResult<'token, 'a>

  let bind f p =
    fun input ->
      match p input with
      | Fail e -> Fail e
      | Success (parsed, rest) ->
        f parsed rest

  let unit x =
    fun input -> Success (x, input)
          
  let bindFail f p =
    fun input ->
      match p input with
      | Success (parsed, input) -> Success (parsed, input)
      | Fail e ->
        f e input

  let unitFail e =
    fun _input -> Fail e

  type ParserBuilder() =
    member this.Bind(p: Parser<'token, 'a>, f: 'a -> Parser<'token, 'b>) =
      bind f p
    member this.Return(x): Parser<'token, 'a> =
      unit x
    member this.ReturnFrom(x): Parser<'token, 'a> =
      x
  let parse = ParserBuilder()

  let map f p =
    parse {
      let! parsed = p
      return f parsed
    }

  let withError errorMsgs parser =
    parser |> (bindFail
      (fun msgs -> unitFail <| msgs @ errorMsgs))

  let replaceError errorMsgs parser =
    parser |> (bindFail
      (fun _msgs -> unitFail <| errorMsgs))

  let recognize (p: 'token -> bool): Parser<'token, 'token> =
    fun s ->
      if Seq.isEmpty s then
        Fail [$"Expected token, but reached end of input"]
      else
        let input = Seq.head s
        let rest = Seq.tail s
        if p input then
          Success (input, rest)
        else
          let error =
            s |> Seq.map (fun x -> x.ToString())
            |> Seq.toList
          Fail [$"Couldn't find token in {error}"]

  let accept token: Parser<'token, 'token> =
    recognize ((=) token)
    |> withError [$"Expected {token}"]

  let string (s: string): Parser.Parser<char, string> =
    parse {
      let! chars =
        Combinator.chain (s |> Seq.toList |> List.map Parser.accept)
      return String.Join("", chars)
    }


module Combinator =

  let drop p: Parser.Parser<'token, unit> =
    Parser.parse {
      let! _ = p
      return ()
    }

  let orElse (p1: Parser.Parser<'token, 'a>) (p2: Parser.Parser<'token, 'a>) =
    p1 |> (Parser.bindFail
      (fun e1 ->
        p2 |> (Parser.bindFail
          (fun e2 -> Parser.unitFail <| e1 @ e2))))

  let andThen (p1: Parser.Parser<'token, 'a list>) (p2: Parser.Parser<'token, 'a>) =
    Parser.parse {
      let! r1 = p1
      let! r2 = p2
      return r1 @ [r2]
    }

  let choose (parsers: Parser.Parser<'token, 'a> list): Parser.Parser<'token, 'a> =
    parsers
    |> List.fold orElse (Parser.unitFail [])

  let chain<'token, 'a> (parsers: Parser.Parser<'token, 'a> list): Parser.Parser<'token, 'a list> =
    parsers
    |> List.fold andThen (Parser.unit [])

  let rec anyNumber p =
    p 
    |> atLeastOnce
    |> Parser.bindFail (fun _msgs -> Parser.unit [])

  and atLeastOnce (p: Parser.Parser<'token, 'a>): Parser.Parser<'token, 'a list> =
    Parser.parse {
      let! parsed = p
      let! restParsed = p |> anyNumber
      return parsed :: restParsed
    }

  let rec exactly times (p: Parser.Parser<'token, 'a>): Parser.Parser<'token, 'a list> =
    Parser.parse {
      if times <= 0 then
        return []
      else
        let! parsed = p
        let! restParsed = p |> exactly (times - 1)
        return parsed :: restParsed
    }

  let optionally (p: Parser.Parser<'token, 'a>): Parser.Parser<'token, 'a option> =
    Parser.parse {
      let! parsed = p
      return Some parsed
    }
    |> Parser.bindFail (fun _msgs -> Parser.unit None)

module Utils

open System
open FSharpParserCombinator

let chars_to_string (cs: char list): string =
    String.Join("", cs)

let string_to_int (i: string) =
    Parser.parse {
        match Int32.TryParse(i) with
        | false, _ -> return! Parser.unitFail [$"Couldn't parse string '{i}' as an integer"]
        | true, i -> return i
    }
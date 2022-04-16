module Utils

open System
open Combinators

let chars_to_string (cs: char list): string =
    String.Join("", cs)

let string_to_int (i: string) =
    parser {
        match Int32.TryParse(i) with
        | false, _ -> return! pReturnFail [$"Couldn't parse string '{i}' as an integer"]
        | true, i -> return i
    }
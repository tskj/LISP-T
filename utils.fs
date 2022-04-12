module Utils

open System
open Combinators

let stringToInt (i: string) =
    parser {
        match Int32.TryParse(i) with
        | false, _ -> return! pReturnFail [$"Couldn't parse string '{i}' as an integer"]
        | true, i -> return i
    }
module Nok

open System

open FSharpParserCombinator
open Utils

let acceptable = [
  "123"
  "100.000"
  "100.000,00"
  "100.000,-"
  "Kr 100 000"
  "kr 100.000,-"
  "Kr. 1000,-"
  "kr. 100.000,-"
  "Kr. 100.00,- NOK"
  "100.00,- NOK"
  "KR 100.00,- NOK"
  "Kr 100 000,-"
  "100 000,-"
  "200 100.23"
  "100,200,000.00"
  "20.999,10"
  "1.234.345.643,-"
  "123.000"
  "123 456"
  "1"
  "0"
  "0,-"
  "0,00"
  "0.00"
  "1234"
  "1003000"
  "100300"
  "1234,-"
  "1234.00"
  "12 900"
]

let unacceptable = [
  "100,00,00"         // two digits in the thousands place
  "100,200.000,00"    // mixed thousands separators
  "100,200.000"       // too many cent digits or mixed thousands
  "kR. 100 000"       // capital R but lower case k
  "kR 100 000"        // same
  "100 000.000"       // too many cent digits
  "100.000,0"         // too few cent digits
  "011.100"           // leading zero
  "100.000,000"       // too many cents digits or mixed separators
  "100 200.499,-"     // mixed thousands separators
  "1123 000"          // thousands separating wrong number of digits
  "1234,0000"         // same
  "123456 000"        // same
  "100.000.1234.123"  // same
  "200 00"            // space can only be a thousands separator, not decimal
  "100.200,00.300"    // can't have decimal separator in the middle duh
  "100,300.200,-"     // mixed thousands separators
  "100,000,00"        // same symbol for thousands and decimal separators
  "100.000.00"        // same
]

let kr: Parser.Parser<char, unit> = 
  Parser.parse {
    do! Combinator.choose [
          Parser.string "Kr"
          Parser.string "kr"
          Parser.string "KR"
        ]
        |> Combinator.drop

    do! Parser.accept '.' |> Combinator.optionally |> Combinator.drop

    do! Parser.accept ' ' |> Combinator.drop

    return ()
  }

let integer_with_thousands: Parser.Parser<char, int> =
  ['0'..'9']
  |> List.map Parser.accept
  |> Combinator.choose
  |> Combinator.atLeastOne
  |> Parser.map chars_to_string
  |> Parser.bind string_to_int

let decimal_separator_with_cents: Parser.Parser<char, int> =
  Parser.parse {
    do! Parser.accept ',' |> Combinator.drop

    let! digits =
      Combinator.choose [
        ['0'..'9']
        |> List.map Parser.accept
        |> Combinator.choose
        |> Combinator.exactly 2
        |> Parser.map chars_to_string

        Parser.string "-"
      ]

    if digits = "-" then
      return 0
    else 

    let! integer =
      string_to_int digits

    return integer
  }

let nok: Parser.Parser<char, unit> =
  Parser.parse {
    do! Parser.accept ' ' |> Combinator.drop

    do! Combinator.choose [
          Parser.string "NOK"
          Parser.string "Nok"
        ]
        |> Combinator.drop

    return ()
  }

let parseNokInCents: Parser.Parser<char, int> =
  Parser.parse {
    do! kr |> Combinator.optionally |> Combinator.drop

    let! integer_part = 
      integer_with_thousands

    let! cent_part = 
      decimal_separator_with_cents 
      |> Combinator.optionally
      |> Parser.map (Option.defaultValue 0)

    do! nok |> Combinator.optionally |> Combinator.drop

    return integer_part * 100 + cent_part
  }

let run () =
  let bla = parseNokInCents "Kr. 1234 NOK"
  printfn "%A" bla
  ()
module Pratt 

open System

open FSharpParserCombinator
open Utils
    
type PrefixOperator =
    | Neg
    
type BinaryOperator =
    | Mul
    | Add
    | Sub
    
type PostfixOperator =
    | Fact
    
type Precedence = int
type Association =
    | Left
    | Right

type BinaryOp = BinaryOperator * Precedence * Association
    
type Expression =
    | Op of PrefixOperator * Expression
    | Op' of PostfixOperator * Expression
    | Binary of BinaryOperator * Expression * Expression
    | Int of int

let run () =
    let input = "-1 + --2 * -3 * (5 + 6) - -(4 - 8) + 9 * ((12 + 13) * (140 + 150))"
    
    let parseWhitespace =
        Parser.accept ' '
        
    let parseInt =
        Parser.parse {
           let! digits =  
                ['0'..'9']
                |> List.map Parser.accept
                |> Combinator.choose
                |> Combinator.atLeastOneTime
                
           let! int =
               digits
               |> fun s -> String.Join("", s)
               |> string_to_int
               
           return Int int
        }
        
    let parseBinaryOperator =
        Combinator.choose [
            Parser.accept '*' |> Parser.map (fun _ -> BinaryOp (Mul, 2, Left))
            Parser.accept '+' |> Parser.map (fun _ -> BinaryOp (Add, 1, Left))
            Parser.accept '-' |> Parser.map (fun _ -> BinaryOp (Sub, 1, Left))
        ]

    let parsePrefixOperator =
        Parser.parse {
            do! Parser.accept '-' |> Combinator.drop
            return Neg
        }

    let parsePostfixOperator =
        Parser.parse {
            do! Parser.accept '!' |> Combinator.drop
            return Fact
        }

    let parseExpression: Parser.Parser<char, Expression> =
        let rec pe precedenceLevel lhs =
            Parser.parse {
                do! parseWhitespace |> Combinator.anyNumberOfTimes |> Combinator.drop
                
                let! (op, opPrecedenceLevel, associativity) = parseBinaryOperator
                
                let lessThan =
                    if associativity = Left then
                        (<=)
                    else
                        (<)
                if (opPrecedenceLevel |> lessThan) precedenceLevel then
                    return! Parser.unitFail ["Operator precedence is not high enough to continue recursing"]
                else
                    do! parseWhitespace |> Combinator.anyNumberOfTimes |> Combinator.drop
                    
                    let! rest = pi opPrecedenceLevel
                    
                    let res = Binary (op, lhs, rest)
                    return! Combinator.choose [
                      pe precedenceLevel res
                      Parser.unit res
                    ]
            }
        and pi l =
            let rec parseAtom () =
                Combinator.choose [
                    parseInt
                    Parser.parse {
                        let! op = parsePrefixOperator
                        let! rest = parseAtom ()
                        return Op (op, rest)
                    }
                    Parser.parse {
                        do! Parser.accept '(' |> Combinator.drop
                        let! atom = pi -1
                        do! Parser.accept ')' |> Combinator.drop
                        return atom
                    }
                ]
            Combinator.choose [
                Parser.parse {
                    do! parseWhitespace |> Combinator.anyNumberOfTimes |> Combinator.drop
                    let! a = parseAtom ()
                    return! pe l a
                }
                parseAtom ()
            ]
        pi -1

    input
    |> parseExpression
    |> printfn "%A" 

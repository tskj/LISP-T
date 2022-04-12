module Pratt 

open System

open Combinators
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

let parse () =
    let input = "-1 + --2 * -3 * (5 + 6) - -(4 - 8) + 9 * ((12 + 13) * (140 + 150))"
    
    let parseWhitespace =
        accept ' '
        
    let parseInt =
        parser {
           let! digits =  
                ['0'..'9']
                |> List.map accept
                |> choose
                |> repeat
                
           let! int =
               digits
               |> fun s -> String.Join("", s)
               |> stringToInt
               
           return Int int
        }
        
    let parseBinaryOperator =
        choose [
            accept '*' |> pMap (fun _ -> BinaryOp (Mul, 2, Left))
            accept '+' |> pMap (fun _ -> BinaryOp (Add, 1, Left))
            accept '-' |> pMap (fun _ -> BinaryOp (Sub, 1, Left))
        ]

    let parsePrefixOperator =
        parser {
            do! accept '-' |> drop
            return Neg
        }

    let parsePostfixOperator =
        parser {
            do! accept '!' |> drop
            return Fact
        }

    let parseExpression: Parser<char, Expression> =
        let rec pe precedenceLevel lhs =
            parser {
                do! parseWhitespace |> many |> drop
                
                let! (op, opPrecedenceLevel, associativity) = parseBinaryOperator
                
                let lessThan =
                    if associativity = Left then
                        (<=)
                    else
                        (<)
                if (opPrecedenceLevel |> lessThan) precedenceLevel then
                    return! pReturnFail ["Operator precedence is not high enough to continue recursing"]
                else
                    do! parseWhitespace |> many |> drop
                    
                    let! rest = pi opPrecedenceLevel
                    
                    let res = Binary (op, lhs, rest)
                    return! choose [
                      pe precedenceLevel res
                      parser {
                          return res
                      }
                    ]
            }
        and pi l =
            let rec parseAtom () =
                choose [
                    parseInt
                    parser {
                        let! op = parsePrefixOperator
                        let! rest = parseAtom ()
                        return Op (op, rest)
                    }
                    parser {
                        do! accept '(' |> drop
                        let! atom = pi -1
                        do! accept ')' |> drop
                        return atom
                    }
                ]
            choose [
                parser {
                    do! parseWhitespace |> many |> drop
                    let! a = parseAtom ()
                    return! pe l a
                }
                parseAtom ()
            ]
        pi -1

    input
    |> Seq.toList
    |> parseExpression
    |> printfn "%A" 

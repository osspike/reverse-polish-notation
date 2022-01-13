module Infix

open Domain

exception ExcessessiveToken
exception NotEnoughTokens

type Ast =
    | Expression of string
    | And of Ast * Ast
    | Or of Ast * Ast

let rec parse =
    function
    | [] -> failwith "Not enough tokens"
    | "or" :: tail -> 
        let right, rest = parse tail
        let left, rest = parse rest
        Or (left, right), rest
    | "and" :: tail ->
        let right, rest = parse tail
        let left, rest = parse rest
        And (left, right), rest
    | expr :: tail -> Expression expr, tail

let rec infix p x  =
    let brackets (x : Ast) = seq { yield Infix.Open; yield! infix 0 x; yield Infix.Close }
    seq {
        match x with
        | Expression expr -> yield Infix.Expression expr
        | Or (l, r) when p <= 2 -> yield! infix 2 l; yield Infix.Or; yield! infix 2 r
        | And (l, r) when p <= 3 -> yield! infix 3 l; yield Infix.And; yield! infix 3 r
        | _ -> yield! brackets x
    }

let fromPostfix (input: string list) =
    let (tree, rest) = 
        input
        |> List.rev 
        |> parse
    if not rest.IsEmpty then raise ExcessessiveToken
    infix 0 tree |> Seq.toList
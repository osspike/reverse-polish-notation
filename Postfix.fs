module Postfix

open Domain

exception MissmatchedParantheses
exception IllegalOperation

// Maps first element in the tuple
let map1 f (a, b) = f a, b
// Maps second element in the tuple
let map2 f (a, b) = a, f b
// Appends element to sequence
let append el seq = Seq.append seq [el]
// Parses single token
let parse = function
    | "and" -> Infix.And
    | "or" -> Infix.Or
    | "(" -> Infix.Open
    | ")" -> Infix.Close
    | expr -> Infix.Expression expr

let rec popUntilOpen = function
    | [] -> raise MissmatchedParantheses
    | Infix.Close :: _ -> raise MissmatchedParantheses
    | Infix.Open :: stack -> stack, Seq.empty
    | Infix.And :: stack -> popUntilOpen stack |> map2 (append And)
    | Infix.Or :: stack -> popUntilOpen stack |> map2 (append Or)
    | Infix.Expression expr :: stack -> 
        popUntilOpen stack |> map2 (append (Expression expr))

let rec popForAnd = function
    | Infix.And :: stack -> popForAnd stack |> map2 (append Postfix.And)
    | stack -> stack, Seq.empty

let rec popForOr = function
    | Infix.And :: stack -> popForOr stack |> map2 (append And)
    | Infix.Or :: stack -> popForOr stack |> map2 (append Or)
    | stack -> stack, Seq.empty

let push stack token =
    match token with
    | Infix.Expression expr -> stack, seq [Expression expr]
    | Infix.Open -> Infix.Open :: stack, seq []
    | Infix.Close -> popUntilOpen stack
    | Infix.And -> popForAnd stack |> map1 (fun s -> Infix.And :: s)
    | Infix.Or -> popForOr stack |> map1 (fun s -> Infix.Or :: s)

let fromInfix (input: string list) =
    let mutable stack: Infix list = []
    let mutable result: Postfix seq = []
    for token in input do
        let s, r = push stack (parse token)
        stack <- s
        result <- Seq.append result r
    for op in stack do
        result <- match op with
                  | Infix.And -> Seq.append result [And]
                  | Infix.Or -> Seq.append result [Or]
                  | Infix.Expression expr -> Seq.append result [Expression expr]
                  | _ -> raise IllegalOperation
    Seq.toList result
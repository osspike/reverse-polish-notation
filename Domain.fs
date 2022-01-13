module Domain

type Infix = And | Or | Open | Close | Expression of string
type Postfix = And | Or | Expression of string

let printInfix = function
    | Infix.And -> "and"
    | Infix.Or -> "or"
    | Infix.Open -> "("
    | Infix.Close -> ")"
    | Infix.Expression expr -> expr

let printPostfix = function
    | Postfix.And -> "and"
    | Postfix.Or -> "or"
    | Postfix.Expression expr -> expr
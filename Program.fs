
open Domain

let help = """Infix/postfix (reverse Polish) notation converter

Usage: rpn TOKENS

Examples: 
    rpn "A and ( B or C )"
    rpn "A B C or and"
"""

type InputNotation = Infix | Postfix | Illegal

let detectNotation = function
    | _ :: "and" :: _ :: _ 
    | _ :: "or" :: _ :: _ -> Infix
    | _ :: _ :: _ :: _ -> Postfix
    | _ -> Illegal

[<EntryPoint>]
let main args =
    match args with
    | [| input |] ->
        try
            let input = Array.toList <| input.Split([|' '|])
            match detectNotation input with
            | Infix -> 
                Postfix.fromInfix input 
                    |> List.map printPostfix
                    |> List.iter (printf "%s ")
                printf "\n"
                0
            | Postfix ->
                Infix.fromPostfix input
                    |> List.map printInfix
                    |> List.iter (printf "%s ")
                printf "\n"
                0
            | _ -> 
                eprintfn "Unsupported notation"
                1
        with
            | Postfix.MissmatchedParantheses ->
                eprintfn "Missmatched parantheses"
                1
            | Postfix.IllegalOperation ->
                eprintfn "Illegal operation"
                1
            | Infix.ExcessessiveToken ->
                eprintfn "Excessessive token"
                1
            | Infix.NotEnoughTokens ->
                eprintfn "Not enough tokens"
                1
    | _ -> 
        printfn "%s" help
        0

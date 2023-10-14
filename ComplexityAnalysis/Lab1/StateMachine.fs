module ComplexityAnalysis.Lab1.StateMachine

type Path =
    | Next of char
    | Self of char

// (dd)+a*c+b+(m|n)*
let states = [|
     [| Path.Next 'd'|];
     [| Path.Next 'd'|];
     [| Path.Next 'a'|]
     [| Path.Next 'c'; Path.Self 'a' |]
     [| Path.Next 'b' |];
     [| Path.Next 'm'; Path.Next 'n' |];
     [| Path.Self 'm'; Path.Self 'n' |]
|]

let rec isMatchHelper (str: string) stateIndex pathIndex =
    if str.Length = 0 && stateIndex = states.Length-1 then
        true
    elif pathIndex >= states[stateIndex].Length then
        false
    elif str.Length = 0 then
        false
    else
        match states[stateIndex][pathIndex] with
        | Next expected when str[0] = expected ->
            isMatchHelper (str.Substring(1)) (stateIndex+1) pathIndex
        | Self expected when str[0] = expected ->
            isMatchHelper (str.Substring(1)) stateIndex 0
        | _ ->
            isMatchHelper str stateIndex (pathIndex + 1)

let isMatch (str: string) =
    isMatchHelper str 0 0
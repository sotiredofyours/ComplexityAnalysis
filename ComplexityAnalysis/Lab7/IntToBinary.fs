module ComplexityAnalysis.Lab7.IntToBinary

let rec toBinary (num:int) =
    match num with
    | 0 -> ""
    | _ -> (toBinary (num / 2)) + (string (num % 2))
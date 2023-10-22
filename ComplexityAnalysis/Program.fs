open ComplexityAnalysis.Lab3.Search
open ComplexityAnalysis.Lab2.QuickSort

let printResult (value:option<int>*int) =
    match value with
    | Some index, iteration -> printfn $"Value was found at {index} place, iterations: {iteration}"
    | None, iteration -> printfn $"Value was not found, iterations: {iteration}"

let arr30 = generateList 10 |> List.toArray
let arr200 = generateList 200 |> List.toArray
printfn "Simple search, size: 30"
printResult <| simpleSearch 1 arr30
printfn "Simple search, size: 200"
printResult <| simpleSearch 1 arr200

let sortedArr30 = quicksort (arr30 |> Array.toList) |> List.toArray
let sortedArr200 = quicksort (arr200 |> Array.toList) |> List.toArray

printfn "Simple search, size: 30"
printResult <| simpleSearch 1 sortedArr30
printfn "Binary search, size: 30"
printResult <| binarySearch 1 sortedArr30
printfn "Simple search, size: 200"
printResult <| simpleSearch 1 sortedArr200
printfn "Binary search, size: 200"
printResult <| binarySearch 1 sortedArr200
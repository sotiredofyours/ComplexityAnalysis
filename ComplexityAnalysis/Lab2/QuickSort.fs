module ComplexityAnalysis.Lab2.QuickSort

open System

let rec quicksort list =
   match list with
   | [] -> []                            
   | firstElem::otherElements ->      
        let smallerElements =         
            otherElements
            |> List.filter (fun e -> e < firstElem)
            |> quicksort              
        let largerElements =          
            otherElements
            |> List.filter (fun e -> e >= firstElem)
            |> quicksort              
            
        List.concat [smallerElements; [firstElem]; largerElements]
        
let generateList length =
    let a, b = -999, 999 
    let rnd = Random()
    List.init length (fun _ -> rnd.Next(a, b))
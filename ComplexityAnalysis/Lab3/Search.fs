module ComplexityAnalysis.Lab3.Search

let mutable iteration = 0
let binarySearch (value: int) (array: array<int>) =
    iteration <- 0
    let rec loop lo hi =
        if lo <= hi then
            iteration <- iteration + 1
            let mid = lo + ((hi - lo) >>> 1)
            match array[mid] with
            | x when x = value -> Some mid, iteration
            | x when x < value -> loop (mid + 1) hi
            | _ -> loop lo (mid - 1)
        else None, iteration
    loop 0 (array.Length - 1)
    
let simpleSearch (value:int) (array: 'T[]) =
    iteration <- 0
    let rec loop i =
            iteration <- iteration + 1
            if i >= array.Length then
                None, iteration-1
            else if value = array[i] then
                Some i, iteration-1
            else
                loop (i + 1)
    loop 0
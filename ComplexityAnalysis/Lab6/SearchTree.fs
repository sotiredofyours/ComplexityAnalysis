module ComplexityAnalysis.Lab6.SearchTree

type Node =
    | Leaf
    | Node of float * Node * Node

let rec insert x tree =
    match tree with
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (value, left, right) ->
        if x < value then
            Node (value, insert x left, right)
        else
            Node (value, left, insert x right)

let rec count_levels tree =
    let rec count_nodes_at_level level = function
        | Leaf -> 0
        | Node (_, left, right) ->
            match level with
            | 0 -> 1
            | _ -> count_nodes_at_level (level - 1) left + count_nodes_at_level (level - 1) right
    let rec calculate_depth = function
        | Leaf -> 0
        | Node (_, left, right) -> 1 + max (calculate_depth left) (calculate_depth right)
    let depth = calculate_depth tree
    [0..depth-1] |> List.map count_nodes_at_level |> List.map (fun x -> x.ToString)

let rec delete value = function
    | Leaf -> Leaf
    | Node (x, left, right) ->
        if value = x then
            match left, right with
            | _, Leaf -> left
            | Leaf, _ -> right
            | _, _ -> 
                let min_node, new_left = remove_min right
                Node (min_node, left, new_left)
        else if value < x then
            Node (x, delete value left, right)
        else
            Node (x, left, delete value right)

and remove_min = function 
    | Leaf -> failwith "invalid argument: Tree has no elements"
    | Node (x, Leaf, _) -> x, Leaf
    | Node (x, left, right) -> 
        let min, new_left = remove_min left
        min, (Node(x, new_left, right))

let rec delete_all xs tree =
    match xs with
    | [] -> tree
    | x::xs -> delete_all xs (delete x tree)

let rec printTree indent = function
    | Leaf -> 
        printfn $"%s{indent} -"
    | Node (value, left, right) -> 
        printfn $"%s{indent}|-- %g{value}"
        printTree (indent + "|  ") left
        printTree (indent + "   ") right

let print tree =
    printTree "" tree
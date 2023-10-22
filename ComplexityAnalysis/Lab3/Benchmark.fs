module ComplexityAnalysis.Lab3.Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open ComplexityAnalysis.Lab2.QuickSort

type Benchmarks() =
    
    [<Params(30, 200)>]
    member val size = 0 with get, set
    
    member val iterations = Search.iteration with get, set
    
    [<Benchmark>]
    member this.SimpleSearch() =
        let arr = generateList this.size |> List.toArray
        Search.simpleSearch 0 arr
        
    [<Benchmark>]
    member this.SimpleSearchWithSort() =
        let arr = generateList this.size |> List.toArray
        let sortedArr = quicksort (arr |> Array.toList) |> List.toArray
        Search.simpleSearch 0 sortedArr
    
    [<Benchmark>]
    member this.BinarySearchWithSort() =
        let arr = generateList this.size |> List.toArray
        let sortedArr = quicksort (arr |> Array.toList) |> List.toArray
        Search.binarySearch 0 sortedArr
  
BenchmarkRunner.Run<Benchmarks>() |> ignore
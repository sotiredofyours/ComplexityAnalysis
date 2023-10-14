module ComplexityAnalysis.Lab2.Main

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open ComplexityAnalysis.Lab2.QuickSort

type Benchmarks() =
    [<Params(100, 1000, 10000, 100000)>]
    member val size = 0 with get, set

    [<Benchmark(Baseline = true)>]
    member this.Sort() =
        generateList this.size |> quicksort
  
BenchmarkRunner.Run<Benchmarks>() |> ignore
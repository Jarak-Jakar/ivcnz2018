// Learn more about F# at http://fsharp.org
module Benchmarks

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open SixLabors.Memory
open SixLabors.ImageSharp.PixelFormats
open BenchmarkDotNet.Exporters
open BenchmarkDotNet.Exporters.Csv
open BenchmarkDotNet.Columns
open BenchmarkDotNet.Reports

// Much of this is basically copied/adapted from https://github.com/fsharp/fsharp/blob/master/tests/scripts/array-perf/array-perf.fs

type PerfConfigDivideBy2 () =
    inherit ManualConfig ()
    do
        //base.Add (Job.RyuJitX64.WithGcServer(true).WithGcForce(true).WithGcConcurrent(true))
        base.Add (Job.RyuJitX64.WithGcForce(true).WithGcConcurrent(true))
        base.Add (MemoryDiagnoser.Default)
        base.Add (DisassemblyDiagnoser.Create(new DisassemblyDiagnoserConfig(printIL = true, printSource = true, printAsm = true)))
        base.Add (MarkdownExporter.GitHub)
        let ss = SummaryStyle.Default
        ss.PrintUnitsInHeader <- true
        ss.PrintUnitsInContent <- false
        base.Add(CsvExporter(CsvSeparator.Comma, ss))
        base.Add StatisticColumn.Min

[<Config (typeof<PerfConfigDivideBy2>)>]
type PerfBenchmarkDivideBy2 () =

    //[<Params (5, 7, 9)>]
    [<Params (5, 7)>]
    member val public windowSize = 3 with get, set

    [<Benchmark(Baseline=true,Description="DivisionOperator")>]
    member self.DivisionOperator () =
        (self.windowSize - 1) / 2 |> ignore

     [<Benchmark(Description="RightShift")>]
     member self.RightShift () =
         (self.windowSize - 1) >>> 1 |> ignore


[<EntryPoint>]
let main argv =

    let switch = BenchmarkSwitcher [|typeof<PerfBenchmarkDivideBy2>|]

    switch.Run argv |> ignore

    0 // return an integer exit code

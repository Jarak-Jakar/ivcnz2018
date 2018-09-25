// Learn more about F# at http://fsharp.org
module benchmarks

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

type PerfConfig () =
    inherit ManualConfig ()
    do
        base.Add (Job.RyuJitX64.WithGcServer(true).WithGcForce(true).WithGcConcurrent(true))
        base.Add (MemoryDiagnoser.Default)
        base.Add (MarkdownExporter.GitHub)
        let ss = SummaryStyle.Default
        ss.PrintUnitsInHeader <- true
        ss.PrintUnitsInContent <- false
        base.Add(CsvExporter(CsvSeparator.Comma, ss))
        base.Add StatisticColumn.Min

[<Config (typeof<PerfConfig>)>]
type PerfBenchmark () =

    let mutable intensities = [||]
    let mutable imgWidth = 5
    let mutable imgHeight = 5
    let mutable out_img = new Image<Rgb24>(imgWidth, imgHeight)

    //[<Params (3, 5, 7, 9, 11)>]
    [<Params (3, 5, 7)>]
    //[<Params (3)>]
    member val public windowSize = 0 with get, set

    //[<Params ("very small", "small", "medium", "peppers_gray", "big", "very big")>]
    [<Params ("very small", "small", "medium", "peppers_gray")>]
    //[<Params ("very small")>]
    member val public filename = "" with get, set

    [<GlobalSetup>]
    member self.SetupData () =
        use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Inputs\" + self.filename + "_noisy.png")
        img.Mutate(fun x -> x.Grayscale() |> ignore)
        intensities <- img.GetPixelSpan().ToArray() |> Array.map (fun p -> p.R)
        imgWidth <- img.Width
        imgHeight <- img.Height
        img.Dispose()


    [<Benchmark(Baseline=true,Description="naive")>]
    member self.naive () =
        out_img <- naive.medianFilter intensities imgWidth imgHeight self.windowSize

     [<Benchmark(Description="Braunl")>]
     member self.Braunl () =
         out_img <- Braunl.medianFilter intensities imgWidth imgHeight self.windowSize

     [<Benchmark(Description="cml")>]
     member self.cml () =
         out_img <- cml.medianFilter intensities imgWidth imgHeight self.windowSize


[<EntryPoint>]
let main argv =

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    let switch = BenchmarkSwitcher [|typeof<PerfBenchmark>|]

    switch.Run argv |> ignore

    0 // return an integer exit code

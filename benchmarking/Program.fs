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

//[<MemoryDiagnoser>]
//[<CoreJob>]
type PerfConfig () =
    inherit ManualConfig ()
    do
        base.Add (Job.RyuJitX64.WithGcServer(true).WithGcForce(true).WithGcConcurrent(true))
        base.Add (MemoryDiagnoser.Default)
        //base.Add Job.Core
        //base.Add (Job.MediumRun.WithGcServer(true).WithGcForce(true))
        base.Add (MarkdownExporter.GitHub)
        let ss = SummaryStyle.Default
        ss.PrintUnitsInHeader <- true
        ss.PrintUnitsInContent <- false
        base.Add(CsvExporter(CsvSeparator.Comma, ss))
        //base.Add(CsvMeasurementsExporter.WithStyle(ss))
        base.Add StatisticColumn.Min

[<Config (typeof<PerfConfig>)>]
type PerfBenchmark () =

    let mutable intensities = [||]
    let mutable imgWidth = 5
    let mutable imgHeight = 5
    let mutable out_img = new Image<Rgba32>(imgWidth, imgHeight)

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

    //[<GlobalCleanup>]
    //member self.Cleanup () =
    //    out_img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Outputs\cml_" + System.IO.Path.GetFileNameWithoutExtension(self.filename) + "_noisy.png")


    [<Benchmark(Baseline=true,Description="naive")>]
    member self.naive () =
        out_img <- naive.medianFilter intensities imgWidth imgHeight self.windowSize
        //out_img <- Image.LoadPixelData(res, imgWidth, imgHeight)

     [<Benchmark(Description="Braunl")>]
     member self.Braunl () =
         out_img <- Braunl.medianFilter intensities imgWidth imgHeight self.windowSize
         //out_img <- Image.LoadPixelData(res, imgWidth, imgHeight)

     [<Benchmark(Description="cml")>]
     member self.cml () =
         out_img <- cml.medianFilter intensities imgWidth imgHeight self.windowSize
         //out_img <- Image.LoadPixelData(res, imgWidth, imgHeight)


[<EntryPoint>]
let main argv =
    //let filename = argv.[0]
    ////let numIterations = int argv.[1]
    ////let windowSize = int argv.[2]

    //let filenames = ["very small"; "small"; "medium"; "peppers_gray"; "big"; "very big"]
    //let windowSizes = [3; 5; 7; 9; 11]

    //use img = Image.Load(@"..\..\Images\Inputs\" + filename)
    //img.Mutate(fun x -> x.Grayscale() |> ignore)
    //let intensities = img.GetPixelSpan().ToArray() |> Array.map (fun p -> p.R)  // In grayscale all of R, G, and B should be the same, so can just work with R

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    let switch = BenchmarkSwitcher [|typeof<PerfBenchmark>|]

    switch.Run argv |> ignore

    //printfn "Hello World from F#!"
    0 // return an integer exit code

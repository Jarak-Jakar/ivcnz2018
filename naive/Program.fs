// Learn more about F# at http://fsharp.org
module Naive

open System
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.Formats.Png
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.Memory

type Globals<'a> =
    { width : int // Image width
      height : int // Image height
      windowSize : int // Size of rectangular window to filter over
      offset : int // The distance to either side of the central pixel (vertically and horizontally) to search for the median
      intensities : 'a [] // The single-value intensity measures for each pixel
                          }

type Coordinate = ValueTuple<int, int>

type ProcessWindow<'a> = Globals<'a> -> Coordinate -> 'a []

type MedianFilter<'a> = Globals<'a> -> Image<Rgb24>

let timer = System.Diagnostics.Stopwatch()

let inline findMedian (medianCandidates : ^a []) =
    Array.sortInPlace medianCandidates
    medianCandidates.[(Array.length medianCandidates) >>> 1]

let processWindow globals (struct (x, y) : Coordinate) =

    let lhb = max 0 (x - globals.offset) // Lower horizontal bound
    let uhb = min (globals.width - 1) (x + globals.offset) // Upper horizontal bound
    let lvb = max 0 (y - globals.offset) // Lower vertical bound
    let uvb = min (globals.height - 1) (y + globals.offset) // Upper vertical bound

    let medianCandidates = Array.zeroCreate ((uhb - lhb + 1) * (uvb - lvb + 1))
    let mutable idx = 0

    for w in lvb..uvb do
        for z in lhb..uhb do
            medianCandidates.[idx] <- globals.intensities.[z + globals.width * w]
            idx <- idx + 1

    medianCandidates

let inline makeRgb24 r = Rgb24(r, r, r)

let medianFilter globals =
    let pw = processWindow globals

    let outputPixels =
        Array.Parallel.map (fun i ->
            //let outputPixels = Array.map (fun i ->
            let mutable x = 0
            let y = Math.DivRem(i, globals.width, &x)
            let coord = struct (x, y)

            pw coord
            |> findMedian
            |> makeRgb24) [| 0..globals.intensities.Length - 1 |]

    Image.LoadPixelData(outputPixels, globals.width, globals.height)

[<EntryPoint>]
let main argv =
    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowSize = int argv.[2]
    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling
                                                 ()
    //use img: Image<Rgb24> = Image.Load(@"..\..\Images\Inputs\" + filename)
    use img : Image<Rgb24> =
        Image.Load
            (@"D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Inputs\" + filename)
    img.Mutate(fun x -> x.Grayscale() |> ignore)
    let mutable outImg = null

    let globals =
        { width = img.Width
          height = img.Height
          windowSize = windowSize
          offset = (windowSize - 1) >>> 1 // divide by 2
          intensities = Array.zeroCreate 5 }

    for _ in 0..numIterations do
        timer.Start()
        let inputPixels =
            img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)
        outImg <- medianFilter { globals with intensities = inputPixels }

        timer.Stop()

    //use out_file = new System.IO.FileStream(@"..\..\Images\Outputs\naive_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
    use outFile =
        new System.IO.FileStream(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\Images\Outputs\naive_"
                                 + System.IO.Path.GetFileNameWithoutExtension
                                       (filename) + "_"
                                 + string globals.windowSize + ".png", FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Grayscale
    outImg.Save(outFile, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))
    0 // return an integer exit code

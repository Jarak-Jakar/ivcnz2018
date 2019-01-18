// Learn more about F# at http://fsharp.org
module naive

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.Memory
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats.Png
open System.IO
open Microsoft.FSharp.Core.OptimizedClosures
open System
open System

let timer = System.Diagnostics.Stopwatch()

(* 
    Putting in here my mucking around with trying to do the magic number stuff for speeding up the computation of divisors and remainders
    This is pretty much based on what I read in Hacker's Delight 2nd Edition, specifically in Chapter 10, Figure 10-1
*)

let magic d = 
    let ad = abs(d) |> uint32
    let two31 = 0x80000000u
    let t = two31 + ((uint32 d) >>> 31)
    let anc = t - 1u - (t % ad)
    let mutable p = 31
    let mutable q1 = two31 / anc
    let mutable r1 = two31 - q1 * anc
    let mutable q2 = two31 / ad
    let mutable r2 = two31 - q2 * ad

    // This is utterly horrific, but it was the best way I could come up with to 
    // replicate the do-while loop in Hacker's Delight
    let rec dowhileloop () = 
        p <- p + 1
        q1 <- q1 * 2u
        r1 <- 2u * r1

        if r1 >= anc then
            q1 <- q1 + 1u
            r1 <- r1 - anc

        q2 <- 2u * q2
        r2 <- 2u * r2

        if r2 >= ad then
            q2 <- q2 + 1u
            r2 <- r2 - ad

        let delta = ad - r2

        if q1 < delta || q1 = delta && r1 = 0u then
            dowhileloop()
        else
            ()
    

    let M = if d < 0 then
                (q2 + 1u) |> int
            else
                (q2 + 1u) |> int |> (*) -1

    let s = p - 32
    (M, s)

let quotrem d m s x = 
    let q = (x * m) >>> s
    let t = q * d
    let r = x - t
    (q, r)



let inline findMedian (l: 'a[]) =
    Array.sortInPlace l
    l.[(Array.length l)>>>1]

let processWindow (intensities: 'a[]) width height offset x y =

    let lhb = max 0 (x - offset)
    let uhb = min (width - 1) (x + offset)
    let lvb = max 0 (y - offset)
    let uvb = min (height - 1) (y + offset)

    let meds = Array.zeroCreate ((uhb - lhb + 1) * (uvb - lvb + 1))

    let mutable idx = 0

    for w in lvb..uvb do
        for z in lhb..uhb do
            meds.[idx] <- intensities.[z + width * w]
            idx <- idx + 1

    meds

let inline makeRgb24 r = Rgb24(r, r, r)

let medianFilter (intensities: byte[]) width height windowSize =

    let offset = (windowSize - 1) >>> 1 // divide by 2

    let pw = processWindow intensities width height offset |> FSharpFunc<_, _, _>.Adapt

    let m, s = magic width
    let quotrem' = quotrem width m s

    //let outputPixels = Array.Parallel.map (fun i -> // These calculations are fixed for the whole array.  Could maybe do some vectorisation of them?
    let outputPixels = Array.map (fun i -> // These calculations are fixed for the whole array.  Could maybe do some vectorisation of them?
                            (* let x = i % width
                            let y = i / width *)
                            let x, y = quotrem' i
                            pw.Invoke(x, y) |> findMedian |> makeRgb24
                        ) [|0..intensities.Length-1|]

    Image.LoadPixelData(outputPixels, width, height)

[<EntryPoint>]
let main argv =

    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowSize = int argv.[2]

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    use img: Image<Rgb24> = Image.Load(@"..\..\Images\Inputs\" + filename)
    img.Mutate(fun x -> x.Grayscale() |> ignore)
    let mutable out_img = null

    for _ in 0..numIterations do

        timer.Start()

        let inputPixels = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)

        out_img <- medianFilter inputPixels img.Width img.Height windowSize

        timer.Stop()

    use out_file = new System.IO.FileStream(@"..\..\Images\Outputs\naive_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowSize +  ".png", FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Rgb

    out_img.Save(out_file, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))

    0 // return an integer exit code

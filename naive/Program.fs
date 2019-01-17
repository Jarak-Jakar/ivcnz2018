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
open System.Buffers

let timer = System.Diagnostics.Stopwatch()

let accessClampedArray (arr: 'a[]) width height x y =
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        None
    else
        Some(arr.[x + width * y])

let inline findMedian (l: 'a[]) =
    Array.sortInPlace l
    l.[(Array.length l)>>>1]

(* let processWindow (clampedArrayFunc: FSharpFunc<_, _, _>) windowSize posBound x y =
    let meds = Array.zeroCreate (windowSize * windowSize)
    let negBound = -posBound
    for w in negBound..posBound do
        for z in negBound..posBound do
            meds.[(z + posBound) * windowSize + (w + posBound)] <- clampedArrayFunc.Invoke((x + z), (y + w))
    Array.choose id meds |> findMedian *)

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

    (* let mutable meds' = List.Empty
    for w in lvb..uvb do
        for z in lhb..uhb do
            meds' <- intensities.[z + width * w] :: meds'

    let meds = List.toArray meds' *)


    (* let meds = [|
        for w in lvb..uvb do
            for z in lhb..uhb do
                yield intensities.[z + width * w]
                |] *)

    (* let mutable ystride = 0
    let meds = [|for w in lvb..uvb do
                        ystride <- w * width
                        yield intensities.[lhb + ystride .. uhb + ystride]
                |] |> Array.concat *)

    //findMedian meds
    let res = findMedian meds
    res

let makeRgb24 r = Rgb24(r, r, r)

let medianFilter (intensities: byte[]) width height windowSize =
    //printfn "offset is %d" offset

    let offset = (windowSize - 1) >>> 1 // divide by 2

    (* let ac = accessClampedArray intensities width height |> FSharpFunc<_,_,_>.Adapt
    let pw = processWindow ac windowSize offset |> FSharpFunc<_, _, _>.Adapt *)

    let pw = processWindow intensities width height offset |> FSharpFunc<_, _, _>.Adapt

    let outputPixels = Array.Parallel.map (fun i -> // These calculations are fixed for the whole array.  Could maybe do some vectorisation of them?
    //let outputPixels = Array.map (fun i -> // These calculations are fixed for the whole array.  Could maybe do some vectorisation of them?
                            let x = i % width
                            let y = i / width
                            pw.Invoke(x, y) |> makeRgb24
                        ) [|0..intensities.Length-1|]

    Image.LoadPixelData(outputPixels, width, height)

[<EntryPoint>]
let main argv =

    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowSize = int argv.[2]

    //printfn "%A %A %A" filename numIterations windowSize

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    let myArrayPool = ArrayPool.Create(windowSize * windowSize, 2 * Environment.ProcessorCount)
    //let myArrayPool = ArrayPool.Shared

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

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

let inline checkNonNull argName arg =
            match box arg with
            | null -> nullArg argName
            | _ -> ()

let valuechoose (chooser: 'T -> 'U ValueOption) (array: 'T[]) =
            checkNonNull "array" array

            let mutable i = 0
            let mutable first = Unchecked.defaultof<'U>
            let mutable found = false
            while i < array.Length && not found do
                let element = array.[i]
                match chooser element with
                | ValueNone -> i <- i + 1
                | ValueSome b -> first <- b; found <- true

            if i <> array.Length then

                //let chunk1 : 'U[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked ((array.Length >>> 2) + 1)
                let chunk1 : 'U[] = Array.zeroCreate ((array.Length >>> 2) + 1)
                chunk1.[0] <- first
                let mutable count = 1
                i <- i + 1
                while count < chunk1.Length && i < array.Length do
                    let element = array.[i]
                    match chooser element with
                    | ValueNone -> ()
                    | ValueSome b -> chunk1.[count] <- b
                                     count <- count + 1
                    i <- i + 1

                if i < array.Length then
                    //let chunk2 : 'U[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (array.Length-i)
                    let chunk2 : 'U[] = Array.zeroCreate (array.Length-i)
                    count <- 0
                    while i < array.Length do
                        let element = array.[i]
                        match chooser element with
                        | ValueNone -> ()
                        | ValueSome b -> chunk2.[count] <- b
                                         count <- count + 1
                        i <- i + 1

                    //let res : 'U[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (chunk1.Length + count)
                    let res : 'U[] = Array.zeroCreate (chunk1.Length + count)
                    Array.Copy(chunk1,res,chunk1.Length)
                    Array.Copy(chunk2,0,res,chunk1.Length,count)
                    res
                else
                    //Microsoft.FSharp.Primitives.Basics.Array.subUnchecked 0 count chunk1
                    Array.sub chunk1 0 count
            else
                Array.empty

let timer = System.Diagnostics.Stopwatch()

let accessClampedArray (arr: 'a[]) width height x y =
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        ValueNone
    else
        ValueSome(arr.[x + width * y])

(* let accessClampedArray (arr: 'a[]) width height x y =
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        None
    else
        Some(arr.[x + width * y]) *)

let inline findMedian (l: 'a[]) =
    Array.sortInPlace l
    l.[(Array.length l)>>>1]

(* let processWindow (clampedArrayFunc: FSharpFunc<_, _, _>) windowSize posBound x y =
    let meds = Array.zeroCreate (windowSize * windowSize)
    let negBound = -posBound
    for w in negBound..posBound do
        for z in negBound..posBound do
            meds.[(z + posBound) * windowSize + (w + posBound)] <- clampedArrayFunc.Invoke((x + z), (y + w))
    valuechoose id meds |> findMedian
    //Array.choose id meds |> findMedian *)

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

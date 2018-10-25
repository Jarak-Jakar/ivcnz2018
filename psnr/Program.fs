// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.Memory
open SixLabors.ImageSharp.Advanced
open System.Numerics
open SixLabors.ImageSharp.Processing

let bmax = Byte.MaxValue |> float
let max2 = bmax * bmax

let timer = System.Diagnostics.Stopwatch()

let psnr (originalImgPath : string) (denoisedImgPath : string) =
    use originalImg = Image.Load(originalImgPath)
    let originalImgArr = originalImg.GetPixelSpan().ToArray() |> Array.map (fun p -> p.R |> uint32)
    let denoisedImgArr = Image.Load(denoisedImgPath).GetPixelSpan().ToArray() |> Array.map (fun p -> p.R |> uint32)

    timer.Start()

    //let bmax = Byte.MaxValue |> int
    //let max2 = bmax * bmax |> float
    let coefficient = (originalImg.Width * originalImg.Height) |> float |> (/) 1.0

    let vorig = Vector(originalImgArr)
    let vdenoise = Vector(denoisedImgArr)

    let diffs = vorig - vdenoise //|> Vector.AsVectorUInt64
    let diffs2 = diffs * diffs
    //let (receiverArr : uint64[]) = Array.zeroCreate originalImgArr.Length
    let receiverArr = Array.zeroCreate originalImgArr.Length
    diffs2.CopyTo(receiverArr)
    let MSE = Array.sum receiverArr |> float |> (*) coefficient
    //if MSE = 0.0 then printfn "it's zero!"
    //let psnr = max2 / MSE |> log10 |> (*) 10.0 |> fun i -> Math.Round(i, 2) *)
    max2 / MSE |> log10 |> (*) 10.0 |> fun i -> Math.Round(i, 2)

[<EntryPoint>]
let main argv =
    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    (* let originalImg = Image.Load(argv.[0])
    originalImg.Mutate(fun x -> x.Grayscale() |> ignore)
    let originalImgArr = originalImg.GetPixelSpan().ToArray() |> Array.map (fun p -> p.R)
    let denoisedImgArr = Image.Load(argv.[1]).GetPixelSpan().ToArray() |> Array.map (fun p -> p.R)

    let bmax = Byte.MaxValue |> int
    let max2 = bmax * bmax |> float
    let coefficient = (originalImg.Width * originalImg.Height) |> float |> (/) 1.0

    let vorig = Vector(originalImgArr)
    let vdenoise = Vector(denoisedImgArr)

    let diffs = vorig - vdenoise |> Vector.AsVectorInt32
    let diffs2 = diffs * diffs
    let receiverArr = Array.zeroCreate originalImgArr.Length
    diffs2.CopyTo(receiverArr)
    let MSE = Array.sum receiverArr |> float |> (*) coefficient
    //if MSE = 0.0 then printfn "it's zero!"
    //let psnr = max2 / MSE |> log10 |> (*) 10.0 |> fun i -> Math.Round(i, 2) *)
    //printfn "psnr = %.2f" psnr

    let filenames = ["very small"; "small"; "medium"; "peppers_gray"]
    let windowsizes = [3; 5; 7; 9; 11]
    let algos = ["Naive"; "Braunl"; "CML"]

    //use csvfile = new System.IO.FileStream("psnrs.csv", System.IO.FileMode.OpenOrCreate)
    use csvfile = new System.IO.StreamWriter(path="psnrs.csv")
    fprintfn csvfile "Algorithm,Image,WindowSize,PSNR,ElapsedTime"

    for fn in filenames do
        for ws in windowsizes do
            for al in algos do
                //timer.Start()
                let ratio = psnr (@"D:\Users\jcoo092\Writing\2018\IVCNZ18\ivcnz2018\Images\Outputs\Gray\" + fn + "_gray.png")
                                    (@"D:\Users\jcoo092\Writing\2018\IVCNZ18\ivcnz2018\Images\Outputs\" + al + @"\" + fn + "_" + al + "_" + string ws + ".png")
                timer.Stop()
                //printfn "%s %s %d %.2f" al fn ws ratio
                fprintfn csvfile "%s,%s,%d,%.2f,%d" al fn ws ratio timer.ElapsedMilliseconds
                //printfn "  - time: %d ms" timer.ElapsedMilliseconds
                timer.Reset()


    //printfn "Hello World from F#!"
    0 // return an integer exit code

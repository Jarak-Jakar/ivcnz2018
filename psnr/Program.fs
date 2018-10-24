// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.Memory
open SixLabors.ImageSharp.Advanced
open System
open System.Numerics

[<EntryPoint>]
let main argv =
    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    let originalImg = Image.Load(argv.[0])
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
    let psnr = max2 / MSE |> log10 |> (*) 10.0 |> fun i -> Math.Round(i, 2)
    printfn "psnr = %.2f" psnr


    //printfn "Hello World from F#!"
    0 // return an integer exit code

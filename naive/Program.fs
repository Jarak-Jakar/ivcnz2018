﻿// Learn more about F# at http://fsharp.org
module naive

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.Memory
open SixLabors.ImageSharp.Processing

let timer = System.Diagnostics.Stopwatch()

let accessClampedArray (arr: 'a[]) width height x y =
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        None
    else
        Some(arr.[x + width * y])

let findMedian l =
    let ls = List.sort l
    l.[List.length l / 2]

let processWindow clampedArrayFunc windowSize x y =
    let mutable intensities = List.Empty
    let posBound = (windowSize - 1) / 2
    let negBound = -posBound
    let mutable p = clampedArrayFunc x y
    for z in negBound..posBound do
        for w in negBound..posBound do
            let intensity = clampedArrayFunc (x + z) (y + w)
            intensities <- intensity :: intensities
    List.choose id intensities |> findMedian

let makeRgba32 r = Rgba32(r, r, r, 255uy)

[<EntryPoint>]
let main argv =

    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowSize = 3

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    use img = Image.Load(@"..\..\Images\Inputs\" + filename)
    img.Mutate(fun x -> x.Grayscale() |> ignore)

    timer.Start()

    let inputPixels = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)

    let ac = accessClampedArray inputPixels img.Width img.Height
    let pw = processWindow ac windowSize

    let outputPixels = Array.Parallel.map (fun i ->
                            let x = i % img.Width
                            let y = i / img.Width
                            pw x y |> makeRgba32
                        ) [|0..inputPixels.Length-1|]

    let out_img = Image.LoadPixelData(outputPixels, img.Width, img.Height)

    timer.Stop()

    out_img.Save(@"..\..\Images\Outputs\naive_" + System.IO.Path.GetFileNameWithoutExtension(filename) + ".png")

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))

    0 // return an integer exit code

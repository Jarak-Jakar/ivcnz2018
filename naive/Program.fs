// Learn more about F# at http://fsharp.org
module naive

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.Memory
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats.Png
open System.IO

let timer = System.Diagnostics.Stopwatch()

let accessClampedArray (arr: 'a[]) width height x y =
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        None
    else
        Some(arr.[x + width * y])

let findMedian l =
    let ls = List.sort l
    ls.[(List.length l) / 2]

let processWindow clampedArrayFunc windowSize x y =
    let mutable intensities = List.Empty
    let posBound = (windowSize - 1) / 2
    let negBound = -posBound
    //let mutable p = clampedArrayFunc x y
    for z in negBound..posBound do
        for w in negBound..posBound do
            (* let intensity = clampedArrayFunc (x + z) (y + w)
            intensities <- intensity :: intensities *)
            intensities <- (clampedArrayFunc (x + z) (y + w)) :: intensities
    List.choose id intensities |> findMedian

//let makeRgba32 r = Rgba32(r, r, r, 255uy)
let makeRgb24 r = Rgb24(r, r, r)

let medianFilter intensities width height windowSize = 
    let ac = accessClampedArray intensities width height
    let pw = processWindow ac windowSize

    let outputPixels = Array.Parallel.map (fun i ->
                            let x = i % width
                            let y = i / width
                            pw x y |> makeRgb24
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

    timer.Start()

    let inputPixels = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)

    //let ac = accessClampedArray inputPixels img.Width img.Height
    //let pw = processWindow ac windowSize

    //let outputPixels = Array.Parallel.map (fun i ->
    //                        let x = i % img.Width
    //                        let y = i / img.Width
    //                        pw x y |> makeRgba32
    //                    ) [|0..inputPixels.Length-1|]

    //let out_img = Image.LoadPixelData(outputPixels, img.Width, img.Height)

    let out_img = medianFilter inputPixels img.Width img.Height windowSize

    timer.Stop()

    //out_img.Save(@"..\..\Images\Outputs\naive_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
    //                "_" + string windowSize +  ".png")

    use out_file = new System.IO.FileStream(@"..\..\Images\Outputs\naive_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowSize +  ".png", FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Rgb

    out_img.Save(out_file, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))

    0 // return an integer exit code

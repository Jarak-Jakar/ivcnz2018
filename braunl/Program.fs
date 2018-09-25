// Learn more about F# at http://fsharp.org
module Braunl

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.Memory
open SixLabors.ImageSharp.Formats.Png

type Gpx = byte option

type Directions =
    | North
    | South
    | West
    | East
    | Northwest
    | Northeast
    | Southwest
    | Southeast

let tryGetGpx (pixelSpan: byte[]) width height x y : Gpx=
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        None
    else
        Some(pixelSpan.[x + width * y])
               
let step magnitude direction = 
    let onem = 1 * magnitude
    let nonem = -1 * magnitude
    match direction with
    | West -> (nonem, 0)
    | East -> (onem, 0)
    | North ->  (0, nonem)
    | South -> (0, onem)
    | Northwest -> (nonem, nonem)
    | Northeast -> (onem, nonem)
    | Southwest -> (nonem, onem)
    | Southeast -> (onem, onem)

let move pixelSpan width height idx direction stepSize =
    let tgg = tryGetGpx pixelSpan width height
    let x = idx % width
    let y = idx / width
    let dx, dy = step stepSize direction
    let a,b = (x + dx), (y + dy)
    tgg a b

let arrayMedian arr =
    let a = Array.sort arr
    a.[a.Length / 2]

let mergeGpxArraysTuple (a: Gpx [], b: Gpx [], c: Gpx []) =
    Array.append a b |> Array.append c |> Array.choose id |> arrayMedian

let mergeGpxArrays (arr: Gpx [][]) =
    Array.reduce Array.append arr |> Array.choose id |> arrayMedian

let inline createRgb24Pixel r =
    Rgb24(r, r, r)

let getNeighbours width height windowSize (neighbourhoods: Gpx[][]) i =
    let x = i % width
    let y = i / width
    let ub = (windowSize - 1) / 2
    let lb = -ub
    let ys = Array.map (fun j -> (y + j) * width + x) [|lb..ub|]

    let nones = Array.zeroCreate windowSize

    Array.map (fun j -> if j < 0 || j > (width * height) - 1 then nones else neighbourhoods.[j]) ys

let medianFilter intensities width height windowSize = 
    let mv = move intensities width height

    let buildNeighbourArray i p =
        let arr = Array.zeroCreate windowSize
        let ub = (windowSize - 1) / 2
        let lb = -ub
        for j in lb..(-1) do
            arr.[j + ub] <- mv i West j
        
        arr.[ub] <- Some(p)

        for j in 1..ub do
            arr.[j + ub] <- mv i East j
        arr

    let gn = getNeighbours width height windowSize <| Array.Parallel.mapi buildNeighbourArray intensities

    let finalPixels = [|0..(width * height)-1|] |> Array.Parallel.map (gn >> mergeGpxArrays >> createRgb24Pixel)

    Image.LoadPixelData(finalPixels, width, height)


[<EntryPoint>]
let main argv =

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    let filename = argv.[0]
    let totalIterations = int argv.[1]
    let windowSize = int argv.[2]
    use img = Image.Load(@"..\..\Images\Inputs\" + filename)
    img.Mutate(fun x -> x.Grayscale() |> ignore)

    let timer = System.Diagnostics.Stopwatch()

    timer.Start()

    let intensities = img.GetPixelSpan().ToArray() |> Array.map (fun p -> p.R)  // In grayscale all of R, G, and B should be the same, so can just work with R

    let out_img = medianFilter intensities img.Width img.Height windowSize

    timer.Stop()

    use out_file = new System.IO.FileStream(@"..\..\Images\Outputs\braunl_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowSize +  ".png", System.IO.FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Rgb
    out_img.Save(out_file, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / float totalIterations)

    0 // return an integer exit code

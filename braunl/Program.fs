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
    //let (a,b) = match direction with
    //            | West -> (x - 1, y)
    //            | East -> (x + 1, y)
    //            | North ->  (x, y - 1)
    //            | South -> (x, y + 1)
    //            | Northwest -> (x - 1, y - 1)
    //            | Northeast -> (x + 1, y - 1)
    //            | Southwest -> (x - 1, y + 1)
    //            | Southeast -> (x + 1, y + 1)
    let a,b = (x + dx), (y + dy)
    tgg a b

let arrayMedian arr =
    let a = Array.sort arr
    a.[a.Length / 2]

let mergeGpxArraysTuple (a: Gpx [], b: Gpx [], c: Gpx []) =
    Array.append a b |> Array.append c |> Array.choose id |> arrayMedian

let mergeGpxArrays (arr: Gpx [][]) =
    //Array.append a b |> Array.append c |> Array.choose id |> arrayMedian
    Array.reduce Array.append arr |> Array.choose id |> arrayMedian

//let inline createRgba32Pixel r =
//    Rgba32(r, r, r, Byte.MaxValue)

let inline createRgb24Pixel r =
    Rgb24(r, r, r)

//let getNeighbours width height (neighbourhoods: Gpx[][]) i =
//    let x = i % width
//    let y = i / width
//    let yLessOne = (y - 1) * width + x
//    let yPlusOne = (y + 1) * width + x

//    let nones = Array.zeroCreate 3

//    let here = neighbourhoods.[x + y * width]
//    let up = if yLessOne < 0 then
//                nones
//             else
//                neighbourhoods.[yLessOne]
//    let down = if yPlusOne > (width * height) - 1 then
//                nones
//               else
//                neighbourhoods.[yPlusOne]
//    (up, here, down)

let getNeighbours width height windowSize (neighbourhoods: Gpx[][]) i =
    let x = i % width
    let y = i / width
    let ub = (windowSize - 1) / 2
    let lb = -ub
    //let yLessOne = (y - 1) * width + x
    //let yPlusOne = (y + 1) * width + x
    let ys = Array.map (fun j -> (y + j) * width + x) [|lb..ub|]

    let nones = Array.zeroCreate windowSize

    //let here = neighbourhoods.[x + y * width]
    //let up = if yLessOne < 0 then
    //            nones
    //         else
    //            neighbourhoods.[yLessOne]
    //let down = if yPlusOne > (width * height) - 1 then
    //            nones
    //           else
    //            neighbourhoods.[yPlusOne]
    //(up, here, down)

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

            //arr.[j + ub] <- mv i West j
            //arr.[1] <- Some(p)
            //arr.[2] <- mv i East j
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
    //let mv = move intensities img.Width img.Height

    //let buildNeighbourArray i p =
    //    let arr = Array.zeroCreate windowSize
    //    arr.[0] <- mv i West
    //    arr.[1] <- Some(p)
    //    arr.[2] <- mv i East
    //    arr

    //let gn = getNeighbours img.Width img.Height <| Array.Parallel.mapi buildNeighbourArray intensities

    //let finalPixels = [|0..(img.Width * img.Height)-1|] |> Array.Parallel.map (gn >> mergeGpxArraysTuple >> createRgba32Pixel)

    //let out_img = Image.LoadPixelData(finalPixels, img.Width, img.Height)

    let out_img = medianFilter intensities img.Width img.Height windowSize

    timer.Stop()

    //out_img.Save(@"..\..\Images\Outputs\braunl_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
    //                "_" + string windowSize +  ".png")

    use out_file = new System.IO.FileStream(@"..\..\Images\Outputs\braunl_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowSize +  ".png", System.IO.FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Rgb
    out_img.Save(out_file, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / float totalIterations)

    0 // return an integer exit code

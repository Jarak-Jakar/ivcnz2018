// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats

type Gpx = byte option

type Move =
    | West
    | East
    | North
    | South
    | Northwest
    | Northeast
    | Southwest
    | Southeast

let tryGetGpx (pixelSpan: byte[]) width height x y : Gpx=
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        None
    else
        Some(pixelSpan.[x + width * y])

let move pixelSpan width height idx direction =
    let tgg = tryGetGpx pixelSpan width height
    let x = idx % width
    let y = idx / width
    let (a,b) = match direction with
                | West -> (x - 1, y)
                | East -> (x + 1, y)
                | North ->  (x, y - 1)
                | South -> (x, y + 1)
                | Northwest -> (x - 1, y - 1)
                | Northeast -> (x + 1, y - 1)
                | Southwest -> (x - 1, y + 1)
                | Southeast -> (x + 1, y + 1)
    tgg a b

let arrayMedian arr =
    Array.Sort arr
    arr.[arr.Length / 2]

let swap (a: 'a byref) (b: 'a byref) =
    let tmp = a
    a <-b
    b <- tmp

let sort3Elems (arr: Gpx[]) =
    if arr.[0] > arr.[1] then swap &arr.[0] &arr.[1]
    if arr.[1] > arr.[2] then swap &arr.[1] &arr.[2]
    if arr.[0] > arr.[1] then swap &arr.[0] &arr.[2]

let mergeGpxArrays (a: Gpx []) (b: Gpx []) (c: Gpx []) =
    let mutable i = 0
    let mutable j = 0
    let mutable k = 0
    let mutable res = Some(Byte.MinValue)
    for _ in 0..4 do
        if a.[i] < b.[j] then
            if a.[i] < c.[k] then
                res <- a.[i]
                i <- i + 1
            else
                res <- c.[k]
                i <- i + 1
        elif b.[j] < c.[k] then
            res <- b.[j]
            j <- j + 1
        else
            res <- c.[k]
            k <- k + 1
    res.Value // Can throw an exception, which is desired behaviour here as there should always be a real pixel value in the median value

let mergeGpxArraysTuple (a: Gpx [], b: Gpx [], c: Gpx []) =
    Array.append a b |> Array.append c |> Array.filter (fun g -> g.IsSome) |> arrayMedian |> fun g -> g.Value

let inline createRgba32Pixel r =
    Rgba32(r, r, r, Byte.MaxValue)

let getNeighbours (neighbourhoods: Gpx[][]) width height i =
    let x = i % width
    let y = i / width
    let yLessOne = (y - 1) * width + x
    let yPlusOne = (y + 1) * width + x

    let nones = Array.zeroCreate 4
    nones.[3] <- Some(Byte.MaxValue)

    let here = neighbourhoods.[x + y * width]
    let up = if yLessOne < 0 then
                nones
             else
                neighbourhoods.[yLessOne]
    let down = if yPlusOne > (width * height) - 1 then
                nones
               else
                neighbourhoods.[yPlusOne]
    (up, here, down)



[<EntryPoint>]
let main argv =

    let img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")
    img.Mutate(fun x -> x.Grayscale() |> ignore)

    let intensities = img.GetPixelSpan().ToArray() |> Array.map (fun p -> p.R)  // In grayscale all of R, G, and B should be the same, so can just work with R
    let mv = move intensities img.Width img.Height

    let buildNeighbourArray i p =
        let arr = Array.zeroCreate 4
        arr.[0] <- mv i West
        arr.[1] <- Some(p)
        arr.[2] <- mv i East
        arr.[3] <- Some(Byte.MaxValue)
        sort3Elems arr
        arr

    let neighbourhoods = Array.mapi buildNeighbourArray intensities
    let gn = getNeighbours neighbourhoods img.Width img.Height

    let finalPixels = [|0..neighbourhoods.Length-1|] |> Array.map (gn >> mergeGpxArraysTuple >> createRgba32Pixel)

    let out_img = Image.LoadPixelData(finalPixels, img.Width, img.Height)

    out_img.Save(@"..\..\median_out.jpg")

    0 // return an integer exit code

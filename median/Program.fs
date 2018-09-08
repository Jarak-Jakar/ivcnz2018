// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced

type gpx = ValueOption<byte>

let bbbb = Some(5)

type Move = 
    | West
    | East
    | North
    | South
    | Northwest
    | Northeast
    | Southwest
    | Southeast

let tryGetGrayPixel (pixelSpan: byte[]) width height x y : gpx= 
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        ValueNone
    else
        ValueSome(pixelSpan.[x + width * y])

let move pixelSpan width height idx direction = 
    let ggp = tryGetGrayPixel pixelSpan width height
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
    ggp a b

let arrayMedian arr = 
    Array.Sort arr
    arr.[arr.Length / 2]

let swap (a: 'a byref) (b: 'a byref) = 
    let tmp = a
    a <-b
    b <- tmp

let sort3Elems (arr: gpx[]) = 
    if arr.[0] > arr.[1] then swap &arr.[0] &arr.[1]
    if arr.[1] > arr.[2] then swap &arr.[1] &arr.[2]
    if arr.[0] > arr.[1] then swap &arr.[0] &arr.[2]


let mergeByteArrays (a: byte[]) (b: byte[]) (c: byte[]) = 
    let mutable i = 0
    let mutable j = 0
    let mutable k = 0
    let mutable res = Byte.MinValue
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
    res

[<EntryPoint>]
let main argv =

    let img = Image.Load(@"..\..\cute-puppy.jpg")
    img.Mutate(fun x -> x.Grayscale() |> ignore)

    //printfn "%A" ((Array.skip 100 (img.GetPixelSpan().ToArray()) |> Array.take 20) |> Array.map (fun p -> p.R))

    let intensities = img.GetPixelSpan().ToArray() |> Array.map (fun p -> p.R)
    //let ggp = getGrayPixelOption intensities img.Width img.Height
    let mv = move intensities img.Width img.Height

    let buildNeighbourArray i p = 
        let arr = Array.zeroCreate 4
        arr.[0] <- mv i West
        arr.[1] <- ValueSome(p)
        arr.[2] <- mv i East
        arr.[3] <- ValueSome(Byte.MaxValue)
        sort3Elems arr

    let neighbourhoods = Array.mapi buildNeighbourArray intensities

    //printfn "Hello World from F#!"
    0 // return an integer exit code

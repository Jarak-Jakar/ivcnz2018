﻿// Learn more about F# at http://fsharp.org

open System
open Hopac
open Hopac.Extensions
open Hopac.Infixes
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced

type 'a Pix = {
    intensity: 'a
    index: int
    neighbours: 'a option list
    chan: 'a option Ch
}

type 'a Choice =
    | Give
    | Take of ('a option * int)

type Directions =
    | North
    | South
    | West
    | East
    | Northwest
    | Northeast
    | Southwest
    | Southeast

let directions = [|North; South; West; East; Northwest; Northeast; Southwest; Southeast|]

let displacement = function
    | North -> (0, -1)
    | South -> (0, 1)
    | West -> (-1, 0)
    | East -> (1,0)
    | Northwest -> (-1, -1)
    | Northeast -> (1, -1)
    | Southwest -> (-1, 1)
    | Southeast -> (1, 1)


let findIndex width x y =
    x + width * y

let findCoords width index =
    (index % width, index / width)

let makeNeighboursIndexList pix coordFinder indexFinder =
    let x,y = coordFinder pix.index
    Array.map (fun d ->
        let dx,dy = displacement d
        indexFinder (x + dx) (y + dy)
    ) directions
    |> Array.toList

let giveIntensity pix =
    (pix.chan *<- Some(pix.intensity))
        ^->. Give

let rec loopGiving pix count =
    job {
        do! (pix.chan *<- Some(pix.intensity))
        return! loopGiving pix (count + 1)
    }

let takeIntensity (pixels: 'a Pix []) neighbourIndex =
    if neighbourIndex < 0 || neighbourIndex >= pixels.Length then
        Alt.once (Take(None, neighbourIndex))
    else
        Ch.take pixels.[neighbourIndex].chan
            ^-> (fun i -> Take (i, neighbourIndex))


let findArrayMedian arr =
    Array.Sort arr
    arr.[arr.Length / 2]

let buildAlts (pixels: 'a Pix []) pix neighbours =
    let takeAlts = List.map (takeIntensity pixels) neighbours
    (giveIntensity pix) :: takeAlts

let makeRgba32 intensity = Rgba32(intensity, intensity, intensity, 255uy)


let runPixel coordFinder indexFinder pixels latch (outputArray: Rgba32 []) pix = job {
//let runPixel coordFinder indexFinder pixels latch oachan pix = job {
    let neighboursList = makeNeighboursIndexList pix coordFinder indexFinder
    let ba = buildAlts pixels pix
    let rec runpix neighbours p = job {

        if List.isEmpty neighbours then
            let median = List.choose id p.neighbours |> Array.ofList |> findArrayMedian
            outputArray.[p.index] <- makeRgba32 median
            //do! oachan *<- (p.index, median)
            do! Latch.decrement latch
            return! (loopGiving p 0)

        else
            let alts = ba neighbours
            let! res = Alt.choose alts
            match res with
            | Give ->
                return! runpix neighbours p
            | Take (n,i) ->
                let newNeighbours = List.except [i] neighbours
                return! runpix newNeighbours {p with neighbours = n :: p.neighbours}
    }
    do! Job.start (runpix neighboursList pix)
    return ()
}

let storeMedians (arr: Rgba32 []) oachan = job {
    let! (index, median) = Ch.take oachan
    arr.[index] <- makeRgba32 median
}

[<EntryPoint>]
let main argv =
    let filename = argv.[0]
    let numIterations = int argv.[1]

    use img = Image.Load(@"..\..\Images\Inputs\" + filename)

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    //let mutable out_img = new Image<Rgba32>(img.Width, img.Height)

    let timer = System.Diagnostics.Stopwatch ()

    for _ in 1..numIterations do

        System.GC.Collect()

        timer.Start ()

        let imageWidth = img.Width
        let imageHeight = img.Height
        let pixelCount = imageWidth * imageHeight
        let intensities = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)
        let fc = findCoords imageWidth
        let fi = findIndex imageWidth
        let barrier = Hopac.Latch pixelCount
        let outputArray = Array.zeroCreate pixelCount
        let pixels = Array.mapi (fun i x -> {intensity = x; index = i; neighbours = [Some(x)]; chan = Ch ()}) intensities
        //let oachan = Ch ()
        let runpix = runPixel fc fi pixels barrier outputArray

        //Job.foreverServer (storeMedians outputArray oachan) |> run

        Array.iter (fun p -> run (Job.start (runpix p))) pixels

        job {do! (Latch.await barrier)} |> run

        //out_img <- Image.LoadPixelData(outputArray, imageWidth, imageHeight)
        let out_img = Image.LoadPixelData(outputArray, imageWidth, imageHeight)
        out_img.Save(@"..\..\Images\Outputs\cml_median_" + System.IO.Path.GetFileNameWithoutExtension(filename) + ".png")

        timer.Stop ()

    //out_img.Save(@"..\..\Images\Outputs\cml_median_" + System.IO.Path.GetFileNameWithoutExtension(filename) + ".png")

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))

    0 // return an integer exit code

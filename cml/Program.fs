// Learn more about F# at http://fsharp.org
module cml

open System
open Hopac
open Hopac.Extensions
open Hopac.Infixes
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced
open Hopac.Extensions
open Hopac

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

(* let rec loopGiving pix count =
    job {
        if pix.gives < 8 || pix.takes < 8 then
            do! (pix.chan *<- Some(pix.intensity))
            return! loopGiving pix (count + 1)
        else
            return ()
    } *)

(* let rec loopGiving pix =
    job {
        if pix.gives < 8 || pix.takes < 8 then
            do! (pix.chan *<- Some(pix.intensity))
            return! loopGiving {pix with gives = (pix.gives + 1)}
        else
            //printfn "Aborting loopGiving"
            Job.abort() |> ignore
    } *)

let takeIntensity pixels neighbourIndex =
    //printfn "taking intensity"
    if neighbourIndex < 0 || neighbourIndex >= (Array.length pixels) then
        Alt.always (Take(None, neighbourIndex))
    else
        Ch.take pixels.[neighbourIndex].chan
            ^-> (fun i -> Take (i, neighbourIndex))


let findArrayMedian arr =
    Array.Sort arr
    arr.[arr.Length / 2]

let inline findListMedian l =
    List.sort l
    |> (fun m -> m.[m.Length / 2])

(* let buildAlts (pixels: 'a Pix []) pix neighbours =
    let takeAlts = List.map (takeIntensity pixels) neighbours
    (giveIntensity pix) :: takeAlts *)

let takeIntensity' pixels neighbours neighbourIndex =
    if neighbourIndex < 0 || neighbourIndex >= (Array.length pixels) then
                Alt.once None
            else
                Ch.take pixels.[neighbourIndex].chan
    //a |> Alt.afterJob (fun x -> )

let buildAlts (pixels: 'a Pix []) pix neighbours =
    //let takeAlts = List.map (takeIntensity pixels) neighbours
    //(giveIntensity pix) :: takeAlts
    let give = pix.chan *<- Some(pix.intensity)
                ^->. Give
    let takes = List.map (takeIntensity pixels) neighbours
    //printfn "takes length is %A" (List.length takes)
    give :: takes

let makeRgba32 intensity = Rgba32(intensity, intensity, intensity, 255uy)


(* let runPixel coordFinder indexFinder pixels latch (outputArray: Rgba32 []) pix = job {
//let runPixel coordFinder indexFinder pixels latch oachan pix = job {
    let neighboursList = makeNeighboursIndexList pix coordFinder indexFinder
    let ba = buildAlts pixels pix
    let rec runpix neighbours p = job {

        if List.isEmpty neighbours then
            //let median = List.choose id p.neighbours |> Array.ofList |> findArrayMedian
            let median = List.choose id p.neighbours |> findListMedian
            outputArray.[p.index] <- makeRgba32 median
            //do! oachan *<- (p.index, median)
            do! Latch.decrement latch
            return! (loopGiving p 0)

        else
            let alts = ba neighbours
            let! res = Alt.choose alts
            match res with
            //match! Alt.choose alts with
            | Give ->
                return! runpix neighbours p
            | Take (n,i) ->
                let newNeighbours = List.except [i] neighbours
                p.neighbours <- n :: p.neighbours
                return! runpix newNeighbours (p)
    }
    do! Job.start (runpix neighboursList pix)
    return ()
} *)

(* let runPixel coordFinder indexFinder pixels latch (outputArray: Rgba32 []) pix =
//let runPixel coordFinder indexFinder pixels latch oachan pix = job {
    let neighboursList = makeNeighboursIndexList pix coordFinder indexFinder
    let ba = buildAlts pixels pix
    //let rec runpix neighbours p = job {
    let rec runpix neighbours p = job {

        if List.isEmpty neighbours then
            //let median = List.choose id p.neighbours |> Array.ofList |> findArrayMedian
            let median = List.choose id p.neighbours |> findListMedian
            outputArray.[p.index] <- makeRgba32 median
            //do! oachan *<- (p.index, median)
            do! Latch.decrement latch
            return! (loopGiving p)

        else
            let alts = ba neighbours
            let! res = Alt.choose alts
            match res with
            //match! Alt.choose alts with
            | Give ->
                return! runpix neighbours ({p with gives = (p.gives + 1)})
            | Take (n,i) ->
                let newNeighbours = List.except [i] neighbours
                //p.neighbours <- n :: p.neighbours
                return! runpix newNeighbours ({p with neighbours = n :: p.neighbours; takes = (p.takes + 1)})
    }
    runpix neighboursList pix *)

let runPixel coordFinder indexFinder pixels barrier (outputArray: Rgba32 []) pix =
//let runPixel coordFinder indexFinder pixels latch oachan pix = job {
    let neighboursIndexList = makeNeighboursIndexList pix coordFinder indexFinder
    let ba = buildAlts pixels pix
    let alts = ba neighboursIndexList
    //let rec runpix neighbours p = job {
    job {
        do! Job.iterateServer (neighboursIndexList, pix, alts) <| fun (neighbours, p, alts) ->
                Alt.choose alts |> Alt.afterFun (fun x ->
                                                    //printfn "pix %d just made a choice" p.index
                                                    match x with
                                                    | Give -> (neighbours, p, alts)
                                                    | Take(n,i) ->
                                                        //printfn "is i in neighbours? %A" (List.contains i neighbours)
                                                        let newNeighbours = List.except [i] neighbours
                                                        //printfn "In pix %d.  neighbours length is %d and newNeighbours length = %d" pix.index (List.length neighbours) (List.length newNeighbours)
                                                        if List.isEmpty newNeighbours then
                                                            let median = List.choose id p.neighbours |> findListMedian
                                                            outputArray.[p.index] <- median |> makeRgba32
                                                            Latch.decrement barrier |> run
                                                            //printfn "decremented the latch"
                                                        else
                                                            ()
                                                        let newAlts = ba newNeighbours
                                                        (newNeighbours, {p with neighbours = n :: p.neighbours}, newAlts)
                                                        (* if List.isEmpty newNeighbours then
                                                            // update median
                                                            let median = List.choose id p.neighbours |> findListMedian
                                                            outputArray.[p.index] <- median |> makeRgba32
                                                            Latch.decrement latch |> run
                                                            //printfn "pix %d just decremented the latch" pix.index
                                                            let onlyAlt = [p.chan *<- Some(p.intensity)
                                                                                ^->. Give]
                                                            ([], p, onlyAlt)
                                                        else
                                                            let newAlts = ba newNeighbours
                                                            (newNeighbours, {p with neighbours = n :: p.neighbours}, newAlts) *)

                )
        return pix
    }
    //runpix neighboursList pix

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
        (* let imageWidth = 50
        let imageHeight = 50
        let pixelCount = imageWidth * imageHeight
        let intensities = Array.init pixelCount byte *)
        let fc = findCoords imageWidth
        let fi = findIndex imageWidth
        let barrier = Hopac.Latch pixelCount
        let outputArray = Array.zeroCreate pixelCount
        let pixels = Array.Parallel.mapi (fun i x -> {intensity = x; index = i; neighbours = [Some(x)]; chan = Ch ();}) intensities
        //let oachan = Ch ()
        let runpix = runPixel fc fi pixels barrier outputArray

        //Job.foreverServer (storeMedians outputArray oachan) |> run

        //timer.Start()

        let rps = Array.Parallel.map runpix pixels

        //printfn "Made it past rps"
        //Array.iter (fun p -> run (Job.start (runpix p))) pixels
        //let res = Job.conCollect rps |> run |> fun r -> r.ToArray()
        Job.conIgnore rps |> run

        //Seq.Con.mapJob (Job.delayWith runpix) pixels |> run |> ignore
        //Seq.Con.mapJob (runpix) pixels |> run |> ignore

        //let rps = Array.Parallel.map (fun p -> Job.delayWith runpix p) pixels
        //Job.conIgnore rps |> run

        //printfn "Made it past conIgnore"
        printfn "Made it past Seq.Con.mapJob"

        //timer.Stop()

        //job {do! (Latch.await barrier)} |> run
        job {do! (Latch.await barrier)} |> run

        printfn "Made it past the barrier"

        //out_img <- Image.LoadPixelData(outputArray, imageWidth, imageHeight)
        let out_img = Image.LoadPixelData(outputArray, imageWidth, imageHeight)
        //out_img.Save(@"..\..\Images\Outputs\cml_median_" + System.IO.Path.GetFileNameWithoutExtension(filename) + ".png")
        timer.Stop ()

    //out_img.Save(@"..\..\Images\Outputs\cml_median_" + System.IO.Path.GetFileNameWithoutExtension(filename) + ".png")

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))

    0 // return an integer exit code

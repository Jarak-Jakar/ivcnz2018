// Learn more about F# at http://fsharp.org

open System
open Hopac
open Hopac.Extensions
open Hopac.Infixes
open SixLabors.ImageSharp.PixelFormats
open Hopac
open Hopac
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
    printfn "giving by pix %d" pix.index
    (pix.chan *<- Some(pix.intensity))
        ^->. Give
    (* pix.chan *<- Some(pix.intensity)
    |> Alt.afterFun (fun _ -> Give) *)

let rec loopGiving pix : Void Job = job {
    do! (pix.chan *<- Some(pix.intensity))
    return! loopGiving pix
}

let takeIntensity neighbour =
    (* Ch.take neighbour.chan
        ^-> (fun i -> Take (i neighbour.index)) *)
    printfn "taking from pix %d" neighbour.index
    Ch.take neighbour.chan
        ^-> (fun i -> Take (i, neighbour.index))


let findArrayMedian arr =
    Array.Sort arr
    arr.[arr.Length / 2]

let buildAlts (pixels: 'a Pix []) pix neighbours =
    (* let neighbourPixels = List.map (fun n -> pixels.[n]) neighbours
    let takeAlts = List.map takeIntensity neighbourPixels *)
    let takeAlts = List.map (fun n -> pixels.[n]) neighbours
                        |> List.map takeIntensity
    (giveIntensity pix) :: takeAlts


let runPixel coordFinder indexFinder pixels latch pix = job {
    // setup goes here
    let neighboursList = makeNeighboursIndexList pix coordFinder indexFinder
    let ba = buildAlts pixels pix
    let rec runpix neighbours p : Void Job = job {
        (* let alts = ba neighbours
        let! res = Alt.choose alts
        match res with
        | Give ->
            return! run p neighbours
        | Take (n,i) ->
            let newNeighbours = List.except [i] neighbours
            return! run {p with neighbours = n :: p.neighbours} newNeighbours *)

        if List.isEmpty neighbours then
            //let median = List.choose id p.neighbours |> Array.ofList |> findArrayMedian
            // send median somewhere
            do! Latch.decrement latch
            return! loopGiving p
            //return! (giveIntensity pix)
        else
            let alts = ba neighbours
            let! res = Alt.choose alts
            match res with
            | Give ->
                return! runpix neighbours p
            | Take (n,i) ->
                let newNeighbours = List.except [i] neighbours
                return! runpix newNeighbours {p with neighbours = n :: p.neighbours}

        //return! run newNeighbours
    }
    do! Job.server (runpix neighboursList pix)
}

[<EntryPoint>]
let main argv =

    (* use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\sample_output.jpg") *)

    let imageWidth = 5
    let imageHeight = 5
    let pixelCount = imageWidth * imageHeight
    let windowSize = 3
    let intensities = Array.init pixelCount byte
    let fc = findCoords imageWidth
    let fi = findIndex imageWidth
    let barrier = Hopac.Latch pixelCount
    let pixels = Array.mapi (fun i x -> {intensity = x; index = i; neighbours = List.empty; chan = Ch ()}) intensities
    let runpix = runPixel fc fi pixels barrier

    //let allJobs = Array.map runpix pixels
    //Array.iter (fun j -> Job.start j |> ignore) allJobs

    Array.map runpix pixels |> Job.conIgnore |> run

    Job.start (job {do! Latch.await barrier}) |> ignore

    printfn "Finished waiting?"

    //printfn "Hello World from F#!"
    0 // return an integer exit code

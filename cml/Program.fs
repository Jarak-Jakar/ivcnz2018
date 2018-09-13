// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Hopac
open Hopac.Core
open Hopac.Extensions
open SixLabors.ImageSharp.PixelFormats
open Hopac

(* type Pix<'a> = {
    pCh: Ch<'a>
} *)

type Pix<'a> = {
    c: Ch<Option<'a>>
    x: int
    y: int
    i: Option<'a>
    n: IVar<Option<'a>> []
}

type Direction =
    | West
    | East
    | North
    | South
    | Northwest
    | Northeast
    | Southwest
    | Southeast

let directions = [|West; East; North; South; Northwest; Northeast; Southwest; Southeast|]

let computeCoords width index =
    (
        index % width,
        index / width
    )

let computeIndex width x y =
    x + y * width

let getDisplacement x y = function
    | West -> (x - 1, y)
    | East -> (x + 1, y)
    | North -> (x, y - 1)
    | South -> (x, y + 1)
    | Northwest -> (x - 1, y - 1)
    | Northeast -> (x + 1, y - 1)
    | Southwest -> (x - 1, y + 1)
    | Southeast -> (x + 1, y + 1)

(* let pix width height idx intensity = job {
    let p = Ch ()
    let (x, y) = computeCoords width idx
    let neighbourIndices = Array.map (fun d -> getDisplacement x y d) directions

    return p
} *)

let createPixel width windowSize index intensity =
    let x, y = computeCoords width index
    let p = {c = Ch (); x = x; y = y; i = Some(intensity); n = Array.create (windowSize * windowSize - 1) (IVar ());}
    //printfn "%A" p.n.[0]
    //IVar.fill p.n.[0] intensity |> run
    p

//val getNeighbourPix: Pix<'a> -> Direction -> Job<'a>

(* let getNeighbourPixelIntensity width (pixelsArray: Pix<'a> []) pixel direction =
    let displaceX, displaceY = getDisplacement pixel.x pixel.y direction
    let neighbourIdx = computeIndex width displaceX displaceY
    job {
        let intensity = Ch.take pixelsArray.[neighbourIdx].c
        return! intensity
    }

let getNeighbourPixelIntensities width pixelsArray pixel =
    Array.map (getNeighbourPixelIntensity width pixelsArray pixel) directions *)

let giveIntensity pix =
    printfn "pix %d %d giving intensity %A" pix.x pix.y pix.i
    Ch.give pix.c pix.i

let lookupNeighbour (pixels: Pix<'a>[]) neighbourIdx =
    if neighbourIdx < 0 || neighbourIdx >= pixels.Length then
        Alt.always None
    else
        pixels.[neighbourIdx].c

let getIntensity pix neighbourChan neighbourNum =
    printfn "pix %d %d taking an intensity from neighbourNum %d" pix.x pix.y neighbourNum

    Ch.take neighbourChan |> Alt.afterJob (fun x -> IVar.fill pix.n.[neighbourNum] x)

let getAllIntensities width (pixels: Pix<'a> []) pix =
    Array.mapi (fun i d ->
                    let displaceX, displaceY = getDisplacement pix.x pix.y d
                    let neighbourIdx = computeIndex width displaceX displaceY
                    printfn "i = %d, neighbourIdx = %d" i neighbourIdx
                    getIntensity pix pixels.[neighbourIdx] i)
                directions

let sendMessages width pixels pix =
    let choices = Array.append (getAllIntensities width pixels pix) [|giveIntensity pix|]
    Job.server << Job.iterate () <| fun () ->
                                    Alt.choose choices
    |> start

let pullOutWindow pix = job {
    Array.iteri (fun i v -> printfn "IVar %d is full? %A" i (IVar.Now.isFull v)) pix.n
    //Array.iteri (fun i v -> printfn "IVar %d contains %A" i (IVar.Now.get v)) pix.n
    return Array.map (fun x -> IVar.read x |> Alt.toAsync) pix.n |> Async.Parallel |> Async.RunSynchronously
    //Array.map (fun x -> IVar.read x |> run) pix.n
    //return! Array.map (fun x -> MVar.read x) pix.n
}

let arrayMedian arr = job {
    let arr2 = Array.choose id arr
    Array.Sort arr2
    return arr2.[arr2.Length / 2]
}

(* let arrayMedian arr =
    Array.Sort arr
    arr.[arr.Length / 2] *)

let findMedians pixArray =
    Array.map (fun x -> pullOutWindow x |> Job.bind arrayMedian |> run) pixArray

//let fms (fmarr: Job<'a>[]) = Array.Parallel.map run fmarr

let makeRgba32 i =
    Rgba32(i, i, i, 255uy)

//let runPixel pixel =
  //  run getNeighbourPixelIntensity width pixelsArray pixel direction

[<EntryPoint>]
let main argv =

    //printfn "Is serverGC? %A" System.Runtime.GCSettings.IsServerGC

    (* use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\sample_output.jpg") *)

    let makePixels = createPixel 5 3 //img.Width
    let intensities = Array.init (5 * 5) byte |> Array.mapi makePixels // temporary only, to give me pixel representations

    let sm = sendMessages 5 intensities
    Array.map sm intensities |> ignore

    let res = findMedians intensities |> Array.Parallel.map makeRgba32 // this part pulls out the results

    printfn "%A" res

    (* let a = intensities.[0].n.[0]
    let b = MVar.fill a 5uy
    let c = Alt.always 5uy


    let d = Ch.give intensities.[0].c intensities.[0].i
    let e = Ch.take intensities.[1].c |> Alt.afterJob (fun x -> MVar.fill intensities.[0].n.[0] x) //|> Alt.afterFun (fun _ -> Alt.zero ())
    let g = Alt.once e

    let p = job {
        return! Ch.take intensities.[1].c |> Alt.afterJob (fun x -> MVar.fill intensities.[0].n.[0] x)
    }

    let q = Alt.once p

    let i = intensities.[0].n
    //let j = Array.map (fun x -> Alt.toAsync x) i |> Async.Parallel |> Async.RunSynchronously
    let k = Array.map (fun x -> MVar.read x |> Alt.toAsync) i |> Async.Parallel |> Async.RunSynchronously


    Alt.choose [d; e; g] |> ignore *)

  (*   let disps = Seq.map (getDisplacement 0 0) directions
                |> Seq.collect (fun (x,y) -> Seq.map (getDisplacement x y) directions)
                |> Seq.distinct
                |> Seq.collect (fun (x,y) -> Seq.map (getDisplacement x y) directions)
                |> Seq.distinct

    printfn "%A %d" disps (Seq.length disps) *)




    //printfn "Hello World from F#!"
    0 // return an integer exit code

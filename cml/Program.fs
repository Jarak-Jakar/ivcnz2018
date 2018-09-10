// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Hopac
open Hopac.Core

(* type Pix<'a> = {
    pCh: Ch<'a>
} *)

type Pix<'a> = {
    c: Ch<'a>
    x: int
    y: int
    i: 'a
}

let computeCoords width index =
    (
        index % width,
        index / width
    )

let computeIndex width x y =
    x + y * width

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

let createPixel width index intensity =
    let x, y = computeCoords width index
    {c = Ch (); x = x; y = y; i = intensity}

//val getNeighbourPix: Pix<'a> -> Direction -> Job<'a>

let getNeighbourPixelIntensity width (pixelsArray: Pix<'a> []) pixel direction = 
    let displaceX, displaceY = getDisplacement pixel.x pixel.y direction
    let neighbourIdx = computeIndex width displaceX displaceY
    job {
        let intensity = Ch.take pixelsArray.[neighbourIdx].c
        return! intensity
    }


let runPixel pixel = 
    run getNeighbourPixelIntensity width pixelsArray pixel direction

[<EntryPoint>]
let main argv =

    (* use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\sample_output.jpg") *)

    let makePixels = createPixel 120
    let intensities = Array.init (120 * 116) byte |> Array.mapi makePixels
    



    printfn "Hello World from F#!"
    0 // return an integer exit code

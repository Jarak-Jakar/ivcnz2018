// Learn more about F# at http://fsharp.org

open System
open Hopac
open Hopac.Extensions
open SixLabors.ImageSharp.PixelFormats

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

let getIntensity (pixels: Pix<'a> []) pix neighbourIdx neighbourNum =
    //printfn "pix %d %d taking an intensity from neighbourNum %d, at index %d" pix.x pix.y neighbourNum neighbourIdx
    (* if neighbourIdx < 0 || neighbourIdx >= pixels.Length then
        Alt.always None |> Alt.afterJob (fun x -> IVar.fill pix.n.[neighbourNum] x)
    else
        Ch.take pixels.[neighbourIdx].c |> Alt.afterJob (fun x -> IVar.fill pix.n.[neighbourNum] x) *)
    let alternative =
        if neighbourIdx < 0 || neighbourIdx >= pixels.Length then
            Alt.once None
        else
            Ch.take pixels.[neighbourIdx].c
    //printfn "Alternative was %A" alternative
    //alternative
    //|> Alt.afterJob (fun x -> //printfn "the chosen alternative is %A" x;
    //                            if not (IVar.Now.isFull pix.n.[neighbourNum]) then IVar.fill pix.n.[neighbourNum] x else Job.unit ())
    alternative |> Alt.afterJob (fun x -> IVar.fill pix.n.[neighbourNum] x)


let getAllIntensities width (pixels: Pix<'a> []) pix =
    Array.mapi (fun i d ->
                    let displaceX, displaceY = getDisplacement pix.x pix.y d
                    let neighbourIdx = computeIndex width displaceX displaceY
                    //printfn "i = %d, neighbourIdx = %d" i neighbourIdx
                    getIntensity pixels pix neighbourIdx  i)
                directions

let sendMessages width pixels pix =
    let choices = Array.append (getAllIntensities width pixels pix) [|giveIntensity pix|]
    //Job.server << Job.iterate () <| fun () ->
    Job.iterateServer () <| fun () ->
                                    Alt.choose choices
    |> start

(* let sendMessages width pixels pix =
    let takeIntensitiesChoices = (getAllIntensities width pixels pix)
    let mutable choices = Array.append takeIntensitiesChoices [|giveIntensity pix|]
    //Job.server << Job.iterate () <| fun () ->
    Job.iterateServer () <| fun () ->
                                    let alternative = Alt.choose choices
                                    printfn "chosen alternative was %A" alternative
                                    choices <- Array.except [|alternative|] choices
                                    alternative

    |> start *)

(* let runPix width pixels pixel = job {
    let takeIntensitiesChoices = Array.toList (getAllIntensities width pixels pixel)
    let rec rp choices = job {
        let allChoices = (giveIntensity pixel) :: choices
        do! (Alt.choose allChoices |> Alt.afterJob (fun c -> rp (List.except [c] allChoices)))
        return! rp choices
    }

    do! rp takeIntensitiesChoices
} *)


let pullOutWindow pix = job {
    //Array.iteri (fun i v -> printfn "IVar %d is full? %A" i (IVar.Now.isFull v)) pix.n
    Array.iteri (fun i v -> if IVar.Now.isFull v then printfn "IVar %d contains %A" i (IVar.read v |> run)) pix.n
    //Array.iteri (fun i v -> printfn "IVar %d contains %A" i (IVar.Now.get v)) pix.n
    return Array.map (fun x -> IVar.read x |> Alt.toAsync) pix.n |> Async.Parallel |> Async.RunSynchronously
    //Array.map (fun x -> IVar.read x |> run) pix.n
    //return! Array.map (fun x -> MVar.read x) pix.n
}

let arrayMedian arr = job {
    let arr2 = Array.choose id arr
    Array.Sort arr2
    printfn "arr2.Length = %d" arr2.Length
    return arr2.[arr2.Length / 2]
}

(* let arrayMedian arr =
    Array.Sort arr
    arr.[arr.Length / 2] *)

let findMedians pixArray =
    Array.map (fun x -> pullOutWindow x |> Job.bind arrayMedian |> run) pixArray

let makeRgba32 i =
    Rgba32(i, i, i, 255uy)

[<EntryPoint>]
let main argv =

    //printfn "Is serverGC? %A" System.Runtime.GCSettings.IsServerGC

    (* use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\sample_output.jpg") *)

    let makePixels = createPixel 5 3 //img.Width
    let intensities = Array.init (5 * 5) byte |> Array.mapi makePixels // temporary only, to give me pixel representations



    //printfn "Hello World from F#!"
    0 // return an integer exit code

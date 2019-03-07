// Learn more about F# at http://fsharp.org
module Cml

open Hopac
open Hopac.Extensions
open Hopac.Infixes
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced
open SixLabors.Memory
open SixLabors.ImageSharp.Formats.Png

[<Struct>]
type Exchange<'a when 'a : unmanaged> =
    | Give
    | Take of System.ValueTuple<'a, int>

//[<Struct>]
type Proxel<'a when 'a : unmanaged> = { // "proxel" is derived as a combination of picture element (pixel) and processing element
// Only intensity and neighbourhoodIntensities should really need refreshing after the proxel has been created
// Currently, the alternatives will also need repopulating, and takesRemaining will need to be reset
    intensity: 'a
    index: int
    neighbourhood: int [] // This keeps a list of the indices in the 1D array of each proxel's neighbour, so that it can simply dive
                            // into the globals.proxels array at that location to get the necessary neighbour
    chan: Ch<'a>
    neighbourhoodIntensities: 'a []
    alternatives: Alt<Exchange<'a>> []
    mutable takesRemaining: uint16
}

type Globals<'a when 'a : unmanaged> = {
    width: int
    height: int
    pixelCount: int
    intensities: 'a []
    mutable proxels: Proxel<'a> []
    windowEdgeSize: int
    offset: int
    barrier: Latch
    finalImageArray: Gray8 []
}

//let makeRgb24 intensity = Rgb24(intensity, intensity, intensity)
let inline makeGray8 intensity = Gray8(intensity)

let inline findArrayMedian l =
    Array.sortInPlace l
    l.[(Array.length l) >>> 1]

let give proxel =
    Ch.give proxel.chan proxel.intensity ^->. Give

let take globals proxel neighbourIndex =
    let neighbour = globals.proxels.[proxel.neighbourhood.[neighbourIndex]]
    Ch.take neighbour.chan ^-> fun received -> Take (System.ValueTuple.Create(received, neighbourIndex))

let determineNeighbours globals index =
    let mutable x = 0
    let y = System.Math.DivRem(index, globals.width, &x)

    let lhb = max 0 (x - globals.offset) // Lower horizontal bound
    let uhb = min (globals.width - 1) (x + globals.offset) // Upper horizontal bound
    let lvb = max 0 (y - globals.offset) // Lower vertical bound
    let uvb = min (globals.height - 1) (y + globals.offset) // Upper vertical bound

    let neighbours = Array.zeroCreate ((uhb - lhb + 1) * (uvb - lvb + 1))
    let mutable idx = 0

    for w in lvb..uvb do
        for z in lhb..uhb do
            neighbours.[idx] <- z + globals.width * w
            idx <- idx + 1

    neighbours


let makeProxel globals index =
    let neighbourhood = determineNeighbours globals index
    let proxelsIntensity = globals.intensities.[index]
    let proxel = {
        intensity = proxelsIntensity
        index = index
        neighbourhood = neighbourhood
        chan = Ch ()
        neighbourhoodIntensities = Array.zeroCreate (Array.length neighbourhood)
        alternatives = null
        takesRemaining = Array.length neighbourhood |> uint16
    }
    proxel.neighbourhoodIntensities.[((Array.length neighbourhood) - 1)] <- proxelsIntensity
    let takes = Seq.init (Array.length neighbourhood) (take globals proxel)
    let alternatives = Seq.append takes (Seq.singleton (give proxel)) |> Seq.toArray
    // Currently the implementation relies on the give being at the end of the array
    // As this way the neighbourIndex returned from a take corresponds directly to the same position in the alternatives list
    {proxel with alternatives = alternatives}

let processExchange prox = function
    | Give -> prox
    | Take(struct (intensity, index)) ->
            prox.neighbourhoodIntensities.[index] <- intensity
            prox.alternatives.[index] <- Alt.never()
            prox.takesRemaining <- prox.takesRemaining - 1us
            prox

let inline storeValueAtEnd globals proxel =
    let finalValue = findArrayMedian proxel.neighbourhoodIntensities |> makeGray8
    globals.finalImageArray.[proxel.index] <- finalValue

let checkFinishedAndSetOutput globals proxel =
    if proxel.takesRemaining = 0us then
        // output finalValue
        storeValueAtEnd globals proxel
        // decrement barrier
        globals.barrier.Decrement() |> run

    proxel

let runProxel globals proxel =
    Job.iterateServer proxel (fun prox ->
                                    Alt.choosy proxel.alternatives |>
                                    Alt.afterFun (processExchange prox) |>
                                    Alt.afterFun (checkFinishedAndSetOutput globals)
    )

let inline makeImage globals =
    Image.LoadPixelData(globals.finalImageArray, globals.width, globals.height)

let medianFilter globals =
    let proxelMaker = makeProxel globals

    let proxels = Array.Parallel.init globals.pixelCount proxelMaker
    globals.proxels <- proxels

    Hopac.Extensions.Array.iterJob (runProxel globals) proxels |> run
    job {do! (Latch.await globals.barrier)} |> run

    makeImage globals

[<EntryPoint>]
let main argv =
    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowEdgeSize = int argv.[2]
    let offset = (windowEdgeSize - 1) >>> 1

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    use img = Image.Load(@"..\..\Images\Inputs\" + filename)

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    let globals = {
        intensities = null
        width = img.Width
        height = img.Height
        pixelCount = img.Width * img.Height
        proxels = null
        windowEdgeSize = windowEdgeSize
        offset = offset
        barrier = Hopac.Latch(img.Width * img.Height)
        finalImageArray = Array.zeroCreate (img.Width * img.Height)
    }

    let mutable outImg = new Image<Gray8>(img.Width, img.Height)

    let timer = System.Diagnostics.Stopwatch ()

    for _ in 1..numIterations do

        System.GC.Collect()

        timer.Start ()

        let imageIntensities = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)

        outImg <- medianFilter {globals with intensities = imageIntensities; barrier = Hopac.Latch(globals.pixelCount)}
        timer.Stop ()

    use outFile = new System.IO.FileStream(@"..\..\Images\Outputs\cml_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowEdgeSize +  ".png", System.IO.FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- System.Nullable(PngColorType.Grayscale)
    outImg.Save(outFile, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))

    0 // return an integer exit code

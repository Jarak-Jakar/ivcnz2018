// Attempt to experiment with using F#'s MailboxProcessor instead of Hopac for doing a message-passing
// based version of the median filter

// Learn more about F# at http://fsharp.org

module Mbp

open System
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.Memory
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats.Png

//[<Struct>]
//type Message<'a> = | Send of 'a

type Proxel<'a> = MailboxProcessor<'a>

type Globals<'a> = {
    width: int
    height: int
    pixelCount: int
    windowEdgeSize: int
    offset: int
    intensities: 'a []
    finalImage: Gray8 []
    mutable proxels: Proxel<'a> []
}

let determineNeighbours globals index =
    let mutable x = 0
    let y = System.Math.DivRem(index, globals.width, &x)

    let lhb = max 0 (x - globals.offset) // Lower horizontal bound
    let uhb = min (globals.width - 1) (x + globals.offset) // Upper horizontal bound
    let lvb = max 0 (y - globals.offset) // Lower vertical bound
    let uvb = min (globals.height - 1) (y + globals.offset) // Upper vertical bound

    let neighbours = Array.zeroCreate ((uhb - lhb + 1) * (uvb - lvb + 1) - 1)
    let mutable idx = 0

    for w in lvb..uvb do
        for z in lhb..uhb do
            let neighbourIndex = z + globals.width * w
            if neighbourIndex <> index then
                neighbours.[idx] <- neighbourIndex
                idx <- idx + 1

    neighbours

let determineMedian intensities =
    Array.sortInPlace intensities
    intensities.[(Array.length intensities) / 2]

let createAgent globals index intensity = MailboxProcessor.Start(fun inbox ->
    let neighbours = determineNeighbours globals index
    let receivedIntensities = Array.zeroCreate (globals.windowEdgeSize * globals.windowEdgeSize)
    receivedIntensities.[0] <- intensity
    let mutable idx = 1
    let mutable sentAnswer = false

    let rec loop neighbours' = async {
        let! msg = inbox.TryReceive 0
        match msg with
        | Some(intensity) ->
            // store value
            receivedIntensities.[idx] <- intensity
            idx <- idx + 1

            if idx >= (Array.length receivedIntensities) then
                // send answer
                sentAnswer <- true

            if sentAnswer && (List.isEmpty neighbours')  then
                return determineMedian receivedIntensities
            else
                return! loop neighbours'

        | None ->
            if sentAnswer && (List.isEmpty neighbours') then
                return determineMedian receivedIntensities
            else
                // post to next neighbour
                return! loop (List.tail neighbours')
    }

    loop neighbours
)

let medianFilter globals =
    ()

[<EntryPoint>]
let main argv =

    let timer = System.Diagnostics.Stopwatch()

    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowSize = int argv.[2]

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    use img: Image = Image.Load(@"..\..\Images\Inputs\" + filename)
    img.Mutate(fun x -> x.Grayscale() |> ignore)
    let mutable outImg = null

    for _ in 0..numIterations do

        timer.Start()

        let inputPixels = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)
        outImg <- medianFilter globals

        timer.Stop()

    use outFile = new System.IO.FileStream(@"..\..\Images\Outputs\mbp_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowSize +  ".png", FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Grayscale

    outImg.Save(out_file, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))
    0 // return an integer exit code

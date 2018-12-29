// Attempt to experiment with using F#'s MailboxProcessor instead of Hopac for doing a message-passing
// based version of the median filter

// Learn more about F# at http://fsharp.org

module mbp

open System
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.Memory
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Formats.Png

let agent = MailboxProcessor<byte>.Start(fun inbox ->
    let neighbours = computeNeighbours i
    let receivedIntensities = Array.zeroCreate (windowSize * windowSize)
    let mutable idx = 0
    let mutable sentAnswer = false

    let rec loop neighbours = async {
        let! msg = inbox.TryReceive 0
        match msg with
        | Some(intensity) -> 
            // store value
            receivedIntensities.[idx] <- intensity
            idx <- idx + 1
            
            if idx >= (Array.length receivedIntensities) then
                // send answer
                sentAnswer <- true
            
            if sentAnswer && (List.isEmpty neighbours)  then
                return ()
            else
                return! loop neighbours

        | None -> 
            if sentAnswer && (List.isEmpty neighbours) then
                return ()
            else
                // post to next neighbour
                return! loop (List.tail neighbours)
    }

    loop neighbours
)

let medianFilter intensities width height windowSize =
    ()

[<EntryPoint>]
let main argv =

    let timer = System.Diagnostics.Stopwatch()

    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowSize = int argv.[2]

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    use img: Image<Rgb24> = Image.Load(@"..\..\Images\Inputs\" + filename)
    img.Mutate(fun x -> x.Grayscale() |> ignore)
    let mutable out_img = null

    for _ in 0..numIterations do

        timer.Start()

        let inputPixels = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)
        out_img <- medianFilter inputPixels img.Width img.Height windowSize

        timer.Stop()

    use out_file = new System.IO.FileStream(@"..\..\Images\Outputs\mbp_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowSize +  ".png", FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Rgb

    out_img.Save(out_file, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))
    0 // return an integer exit code

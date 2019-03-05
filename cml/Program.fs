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

let inline checkNonNull argName arg =
            match box arg with
            | null -> nullArg argName
            | _ -> ()

let valuechoose (chooser: 'T -> ValueOption<'U>) (array: 'T[]) =
            checkNonNull "array" array

            let mutable i = 0
            let mutable first = Unchecked.defaultof<'U>
            let mutable found = false
            while i < array.Length && not found do
                let element = array.[i]
                match chooser element with
                | ValueNone -> i <- i + 1
                | ValueSome b -> first <- b; found <- true

            if i <> array.Length then

                //let chunk1 : 'U[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked ((array.Length >>> 2) + 1)
                let chunk1 : 'U[] = Array.zeroCreate ((array.Length >>> 2) + 1)
                chunk1.[0] <- first
                let mutable count = 1
                i <- i + 1
                while count < chunk1.Length && i < array.Length do
                    let element = array.[i]
                    match chooser element with
                    | ValueNone -> ()
                    | ValueSome b -> chunk1.[count] <- b
                                     count <- count + 1
                    i <- i + 1

                if i < array.Length then
                    //let chunk2 : 'U[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (array.Length-i)
                    let chunk2 : 'U[] = Array.zeroCreate (array.Length-i)
                    count <- 0
                    while i < array.Length do
                        let element = array.[i]
                        match chooser element with
                        | ValueNone -> ()
                        | ValueSome b -> chunk2.[count] <- b
                                         count <- count + 1
                        i <- i + 1

                    //let res : 'U[] = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked (chunk1.Length + count)
                    let res : 'U[] = Array.zeroCreate (chunk1.Length + count)
                    System.Array.Copy(chunk1, res, chunk1.Length)
                    System.Array.Copy(chunk2, 0, res, chunk1.Length, count)
                    res
                else
                    //Microsoft.FSharp.Primitives.Basics.Array.subUnchecked 0 count chunk1
                    Array.sub chunk1 0 count
            else
                Array.empty

// type 'a Pix = {
//     intensity: 'a
//     index: int
//     neighbours: ValueOption<'a> list
//     chan: Ch<ValueOption<'a>>
// }

type 'a Pix = {
    intensity: 'a
    index: int
    neighbourIndices: int []
    neighboursIntensities: 'a []
    chan: Ch<'a>
    iteration: int
}

type 'a Globals = {
    intensities: 'a []
    height: int
    width: int
    windowSize: int
    offset: int
    pixelCount: int
    pixels: 'a Pix []
}

let determineNeighbourIndices globals index =
    let mutable x = 0
    let y = System.Math.DivRem(index, globals.width, &x)
    let lhb = max 0 (x - globals.offset) // Lower horizontal bound
    let uhb = min (globals.width - 1) (x + globals.offset) // Upper horizontal bound
    let lvb = max 0 (y - globals.offset) // Lower vertical bound
    let uvb = min (globals.height - 1) (y + globals.offset) // Upper vertical bound
    let mutable arrayIdx = 0
    let arraySize = min ((uvb - lvb) * (uhb - lhb) - 1) (globals.windowSize * globals.windowSize - 1)
    let indices = Array.zeroCreate arraySize

    for w in lvb..uvb do
        for z in lhb..uhb do
            if w = z then ()
            else
                indices.[arrayIdx] <- z + globals.width * w
                arrayIdx <- arrayIdx + 1

    indices


let createPixelProcess globals index =
    //let intens = globals.intensities.[index]
    let neighbourIndices = determineNeighbourIndices globals index
    {
        intensity = globals.intensities.[index]
        index = index
        neighbourIndices = neighbourIndices
        neighboursIntensities = Array.zeroCreate (Array.length neighbourIndices)
        chan = Ch ()
        iteration = 0
    }

[<Struct>]
type 'a Choice =
    | Give
    | Take of ('a ValueOption * int)

// let listDisplacements ws =
//     let ub = (ws - 1) / 2
//     let lb = -ub
//     let mutable disps = List.empty
//     for i in lb..ub do
//         for j in lb..ub do
//             if i = 0 && j = 0 then
//                 ()
//             else
//                 disps <- (i, j) :: disps
//     disps

// let arrayDisplacements ws =
//     let ub = (ws - 1) / 2
//     let lb = -ub
//     let disps = Array.zeroCreate (ws - 1)
//     let mutable idx = 0
//     for i in lb..ub do
//         for j in lb..ub do
//             if i = 0 && j = 0 then
//                 ()
//             else
//                 disps.[idx] <- (i, j)
//                 idx <- idx + 1
//     disps


let findIndex width x y =
    x + width * y

let findCoords width index =
    (index % width, index / width)

let makeNeighboursIndexList' pix coordFinder indexFinder windowSize =
    let x,y = coordFinder pix.index
    listDisplacements windowSize |>
    List.map (fun (dx, dy) ->
        indexFinder (x + dx) (y + dy)
    )

let makeNeighboursIndexArray' pix coordFinder indexFinder windowSize =
    let x,y = coordFinder pix.index
    arrayDisplacements windowSize |>
    Array.map (fun (dx, dy) ->
        indexFinder (x + dx) (y + dy)
    )

// let takeIntensity pixels neighbourIndex =
//     if neighbourIndex < 0 || neighbourIndex >= (Array.length pixels) then
//         Alt.always (Take(ValueNone, neighbourIndex))
//     else
//         Ch.take pixels.[neighbourIndex].chan
//             ^-> (fun i -> Take (i, neighbourIndex))

let takeIntensity (channels: 'a Ch []) neighbourIndex =
    Ch.take channels.[neighbourIndex] ^-> (fun i -> Take(i, neighbourIndex))


let inline findListMedian l =
    List.sort l
    |> (fun m -> m.[m.Length / 2])

let inline findArrayMedian l =
    Array.sortInPlace l
    l.[(Array.length l) >>> 1]


let buildAlts' globals pix neighbour =
    let give = pix.chan *<- pix.intensity
                ^->. Give
    let take = takeIntensity globals.pixels neighbour
    //[give; take]
    give <|> take

let makeRgb24 intensity = Rgb24(intensity, intensity, intensity)

let runPixel coordFinder indexFinder globals barrier (outputArray: Rgb24 []) pix =
    let neighboursIndexList = makeNeighboursIndexList' pix coordFinder indexFinder globals.windowSize
    let ba = buildAlts' globals pix
    let alts = ba (List.head neighboursIndexList)
    job {
        do! Job.iterateServer ((List.tail neighboursIndexList), pix, alts) <| fun (neighbours, p, alts) ->
                Alt.choose alts |> Alt.afterFun (fun x ->
                                                    match x with
                                                    | Give -> (neighbours, p, alts)
                                                    | Take(n,i) ->
                                                        if List.isEmpty neighbours then
                                                            //let median = List.choose id p.neighbours |> findListMedian
                                                            let median = valuechoose id p.neighbours |> findArrayMedian
                                                            outputArray.[p.index] <- median |> makeRgb24
                                                            Latch.decrement barrier |> run
                                                            ([], p, [pix.chan *<- pix.intensity ^->. Give])
                                                        else
                                                            let newAlts = ba (List.head neighbours)
                                                            ((List.tail neighbours), {p with neighbours = n :: p.neighbours}, newAlts)

                )
        return pix
    }

let storeMedians (arr: Rgb24 []) oachan = job {
    let! (index, median) = Ch.take oachan
    arr.[index] <- makeRgb24 median
}

let medianFilter globals =
    //let pixelCount = globals.width * globals.height
    let fc = findCoords globals.width
    let fi = findIndex globals.width
    let barrier = Hopac.Latch globals.pixelCount
    let outputArray = Array.zeroCreate globals.pixelCount
    let pixelCreator = createPixelProcess globals
    let pixels = Array.Parallel.init globals.pixelCount pixelCreator
    let newGlobals = {globals with pixels = pixels}
    //let pixels = Array.Parallel.mapi (fun i x -> {intensity = x; index = i; neighbours = [ValueSome(x)]; chan = Ch ();}) globals.intensities
    let runpix = runPixel fc fi newGlobals barrier outputArray

    Seq.Con.mapJob runpix pixels |> run |> ignore
    job {do! (Latch.await barrier)} |> run
    Image.LoadPixelData(outputArray, globals.width, globals.height)

[<EntryPoint>]
let main argv =
    let filename = argv.[0]
    let numIterations = int argv.[1]
    let windowSize = int argv.[2]
    //let offset = (windowSize / 2) - 1

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    use img: Image<Rgb24> = Image.Load(@"..\..\Images\Inputs\" + filename)

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    let mutable outImg = new Image<Rgb24>(img.Width, img.Height)

    let timer = System.Diagnostics.Stopwatch ()

    for _ in 1..numIterations do

        System.GC.Collect()

        timer.Start ()

        //let imageWidth = img.Width
        //let imageHeight = img.Height
        //let pixelCount = imageWidth * imageHeight
        let intensities = img.GetPixelSpan().ToArray() |> Array.Parallel.map (fun p -> p.R)
        //outImg <- medianFilter intensities img.Width img.Height windowSize
        let globals = {
            intensities = intensities
            width = img.Width
            height = img.Height
            pixelCount = img.Width * img.Height
            windowSize = windowSize
            offset = (windowSize / 2) - 1
            pixels = null
        }
        outImg <- medianFilter globals
        timer.Stop ()

    use outFile = new System.IO.FileStream(@"..\..\Images\Outputs\cml_" + System.IO.Path.GetFileNameWithoutExtension(filename) +
                    "_" + string windowSize +  ".png", System.IO.FileMode.OpenOrCreate)

    let pngenc = PngEncoder()
    pngenc.ColorType <- PngColorType.Rgb
    outImg.Save(outFile, pngenc)

    let totalTimeTaken = timer.Elapsed.TotalSeconds
    printfn "Total time was %f" totalTimeTaken
    printfn "Average time was %f" (totalTimeTaken / (float numIterations))

    0 // return an integer exit code

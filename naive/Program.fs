// Learn more about F# at http://fsharp.org

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.Memory
open System.Numerics

let accessClampedArrayWithDefault (arr: 'a[]) width height def x y =
    if x < 0 || x > width-1 || y < 0 || y > height-1 then
        def
    else
        arr.[x + width * y]

let extractPixelParts (p: Rgba32) =
    let R = uint32 p.R
    let G = uint32 p.G
    let B = uint32 p.B
    let A = uint32 p.A
    [|R; G; B; A|]

let processWindow clampedArrayFunc windowSize x y = 
    let posBound = (windowSize - 1) / 2
    let negBound = -posBound
    let mutable p = clampedArrayFunc x y
    for z in negBound..posBound do
        for w in negBound..posBound do
            let q = clampedArrayFunc (x + z) (y + w)
            p <- Vector4.Add(p, q)
    Vector4.Divide(p, (windowSize * windowSize |> float32))

[<EntryPoint>]
let main argv =

    let windowSize = argv.[0] |> int

    Configuration.Default.MemoryAllocator <- ArrayPoolMemoryAllocator.CreateWithModeratePooling()

    use img = Image.Load(@"..\..\cute-puppy.jpg")

    let mutable out_img = img.Clone()

    let inputPixels = img.GetPixelSpan().ToArray() |> Array.map (fun p -> p.ToVector4())

    let mutable outputPixels = Array.zeroCreate inputPixels.Length

    let ac = accessClampedArrayWithDefault inputPixels img.Width 
                img.Height (PixelFormats.NamedColors<Rgba32>.Black.ToVector4())
    let pw = processWindow ac windowSize // using default window size of 3 right now

    System.Threading.Tasks.Parallel.For(0, img.Width-1, fun x ->
        for y in 0..img.Height-1 do
            outputPixels.[x + y * img.Width] <- pw x y) |> ignore

    let out_img = Image.LoadPixelData(outputPixels |> Array.map 
                                        (fun n -> Rgba32(n)), img.Width, img.Height)

    out_img.Save(@"..\..\naive_output.jpg")

    img.Dispose()

    0 // return an integer exit code

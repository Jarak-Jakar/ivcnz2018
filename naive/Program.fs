// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats

let zeroClamp width height x y =
    let w = width - 1
    let h = height - 1
    if x < 0 || x > w || y < 0 || x > w then
        (0,0)
    else
        (x,y)

let accessClampedArrayWithDefault arr def x y =
    let w = (Array.length arr) - 1
    let h = (Array.length arr.[0]) - 1
    if x < 0 || x > w || y < 0 || x > w then
        def
    else
        arr.[x].[y]

let inline (+^) (a: PixelFormats.Rgba32) (b: PixelFormats.Rgba32): PixelFormats.Rgba32 =
    let A = a.A + b.A
    let R = a.R + b.R
    let G = a.G + b.G
    let B = a.B + b.G
    Rgba32(R, G, B, A)

let inline (/^) (px: PixelFormats.Rgba32) (divisor: float) =
    let A = float px.A / divisor |> byte
    let R = float px.R / divisor |> byte
    let G = float px.G / divisor |> byte
    let B = float px.B / divisor |> byte
    Rgba32(R, G, B, A)

[<EntryPoint>]
let main argv =
    //printfn "Hello World from F#!"
    use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")

    //printfn "img width is %d, height is %d" img.Width img.Height

    let mutable out_img = img.Clone()

    let pxs = Array.init img.Height (fun x -> img.GetPixelRowSpan(x).ToArray())

    //printfn "pxs.Length is %d, pxs.[0].Length is %d" pxs.Length pxs.[0].Length

    let mutable (nps: PixelFormats.Rgba32[][]) = Array.copy pxs
    let zc = accessClampedArrayWithDefault pxs (Rgba32(0uy, 0uy, 0uy, 0uy))

    for x in 0..img.Height-1 do
        for y in 0..img.Width-1 do
            let p = pxs.[x].[y]
            for z in -1..1 do
                for w in -1..1 do
                    let q = zc (y + z) (x + w)
                    nps.[x].[y] <- nps.[x].[y] +^ q
            nps.[x].[y] <- nps.[x].[y] /^ 9.0
            out_img.[y, x] <- nps.[x].[y]

    out_img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\naive_output.jpg")



    0 // return an integer exit code

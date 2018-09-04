// Learn more about F# at http://fsharp.org

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Advanced
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Formats.Jpeg

(* let zeroClamp width height x y =
    let w = width - 1
    let h = height - 1
    if x < 0 || x > w || y < 0 || x > w then
        (0,0)
    else
        (x,y) *)

let accessClampedArrayWithDefault (arr: uint32[][]) width height def x y : uint32[] =
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

(* let inline (+^) (a: PixelFormats.Rgba32) (b: PixelFormats.Rgba32): PixelFormats.Rgba32 =
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
    Rgba32(R, G, B, A) *)

[<EntryPoint>]
let main argv =
    //printfn "Hello World from F#!"
    use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")

    let mutable out_img = img.Clone()

    let pxs = img.GetPixelSpan().ToArray() |> Array.map extractPixelParts

    let mutable (nps: uint32[][]) = Array.zeroCreate pxs.Length

    let ac = accessClampedArrayWithDefault pxs img.Width img.Height [|0u;0u;0u;0u|]

    for x in 0..img.Width-1 do
        for y in 0..img.Height-1 do
            let p = ac x y
            for z in -1..1 do
                for w in -1..1 do
                    let q = ac (x + z) (y + w)
                    nps.[x + y * img.Width] <- Array.zip p q |> Array.map (fun (a,b) -> a + b)
            nps.[x + y * img.Width] <- Array.map (fun i -> float i / 9.0 |> uint32 ) nps.[x + y * img.Width]

    let abc = Array.collect (fun a -> Array.map byte a) nps

    //printfn "Detected format is %A" (img.GetConfiguration())

    let potato = Image.Load<Rgba32>(img.GetConfiguration(), abc, Formats.Jpeg.JpegDecoder())

    printfn "potato's width is %d and height is %d" potato.Width potato.Height

    (* let zc = accessClampedArrayWithDefault pxs (Rgba32(0uy, 0uy, 0uy, 0uy))

    for x in 0..img.Height-1 do
        for y in 0..img.Width-1 do
            let p = pxs.[x].[y]
            for z in -1..1 do
                for w in -1..1 do
                    let q = zc (y + z) (x + w)
                    nps.[x].[y] <- nps.[x].[y] +^ q
            nps.[x].[y] <- nps.[x].[y] /^ 9.0
            out_img.[y, x] <- nps.[x].[y] *)

    //out_img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\naive_output.jpg")



    0 // return an integer exit code

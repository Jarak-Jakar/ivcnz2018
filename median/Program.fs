// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Advanced

[<EntryPoint>]
let main argv =

    let img = Image.Load(@"..\..\cute-puppy.jpg")
    img.Mutate(fun x -> x.Grayscale() |> ignore)

    printfn "%A" ((Array.skip 100 (img.GetPixelSpan().ToArray()) |> Array.take 20) |> Array.map (fun p -> p.R))

    //printfn "Hello World from F#!"
    0 // return an integer exit code

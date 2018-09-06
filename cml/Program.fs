// Learn more about F# at http://fsharp.org

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Hopac

type Pix<'a> = {
    pCh: Ch<'a>
    sendCount: int
    recvCount: int
}

let stillToComm p =
    if p.sendCount > 0 || p.recvCount > 0 then
        true
    else
        false

let pix v c = job {
    let p = {pCh = Ch ()}
    let rec work v c = job {
        let! m = Alt.choose [
            Ch.take p.pCh ^=> printfn "Received %A"
        ]
    }
}

[<EntryPoint>]
let main argv =

    (* use img = Image.Load(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\cute-puppy.jpg")

    img.Mutate(fun x -> x.Grayscale() |> ignore)

    img.Save(@"D:\Users\jcoo092\Writing\2018\IVCNZ18\sample_output.jpg") *)



    printfn "Hello World from F#!"
    0 // return an integer exit code

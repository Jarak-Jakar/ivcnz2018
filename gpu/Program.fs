// Learn more about F# at http://fsharp.org

open System
open Tensor


let inline cj (x: Tensor<'T>) (j: 'T) =
    x <<<< j |> Tensor.countTrue

let inline ccdf (x: Tensor<'T>) (a: 'T) (b: 'T) =
    //let one = NumericLiteralI.FromOne()
    //let one = 1uy
    //[|a..b|] |> cj x
    Array.Parallel.map (cj x) [|a..b|] |> HostTensor.usingArray

    //HostTensor.Parallel.map (cj x) (HostTensor.arange a one (b + one))

// ccdfSort is directly taken from BIDIMENSIONAL MEDIAN FILTER FOR PARALLEL COMPUTING ARCHITECTURES by Sanchez and Rodriguez, 2012

let inline ccdfSort (x: Tensor<'T>) (a: 'T) (b: 'T) =
    let tau = ccdf x a b
    let y = ccdf tau 0L (x.NElems - 1L)
    y

let getByteMedian arr =
    let medarr = ccdfSort (HostTensor.usingArray arr) 0uy 255uy |> HostTensor.toArray
    medarr.[(Array.length arr) / 2] |> byte
    //let medarr = ccdfSort arr 0uy 255uy
    //medarr.[[|arr.NElems / 2L|]] |> byte

let makeRandomByteArray length =
    let rando = System.Random()
    Array.init length (fun _ -> rando.Next() |> byte)

[<EntryPoint>]
let main _argv =
    let timer = System.Diagnostics.Stopwatch()

    //let mybytes = [|0uy..255uy|]
    let mybytes = makeRandomByteArray 65536 //16777216
    printfn "%A" mybytes
    let idx = (Array.length mybytes) / 2
    //let mybytes' = HostTensor.usingArray mybytes
    timer.Start()
    let ccdfm = getByteMedian mybytes
    timer.Stop()
    printfn "ccdfm = %d, time = %d" ccdfm (timer.ElapsedMilliseconds)

    timer.Restart()
    Array.Sort mybytes
    let reg = mybytes.[idx]
    timer.Stop()

    printfn "reg = %d, time = %d" reg (timer.ElapsedMilliseconds)
    printfn "Hello World from F#!"
    0 // return an integer exit code

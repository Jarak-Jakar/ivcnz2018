(*
    This was taken directly from the source code supplement to Concurrency in .NET by Riccardo Terrell,
    downloaded from the 'free downloads' sidebar at https://www.manning.com/books/concurrency-in-dot-net
    on 16 January 2019.  It has been copied verbatim from the file AsyncObjectPool.fs, which is found
    ..\fConcBook-master\Chapter.13\AsyncObjectPool\AgentObjectPool where fConcBook-master was the top
    directory of the source code

    Many thanks to Mr. Terrell for making this freely available.  If you haven't already, you should really
    take a look at the book.  It's certainly not perfect, but there's a lot of good stuff in there that
    should be of use to pretty much everyone who works in .NET regularly, plus if you nkow nothing about
    parallelism and concurrency it's probably a reasonable introduction to the ideas.
*)

namespace AsyncObjectPool

open System

(* module Say =
    let hello name =
        printfn "Hello %s" name *)

type PoolMessage<'a> =
    | Get of AsyncReplyChannel<'a>
    | Put of 'a
    | GetCount of AsyncReplyChannel<int>

type AgentObjectPool<'a>(generate: Func<'a>, initialPoolCount) =

    let initial = List.init initialPoolCount (fun _ -> generate.Invoke())
    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop(objects) = async {
            let! msg = inbox.Receive()
            match msg with
            | Get(reply) ->
                match objects with
                | head :: tail ->
                    reply.Reply(head)
                    return! loop(tail)
                | [] as empty->
                    reply.Reply(generate.Invoke())
                    return! loop(empty)
            | Put(value)->  return! loop(value :: objects)
            | GetCount(reply) ->
                reply.Reply(objects.Length)
                return! loop(objects)
        }
        loop(initial))

    new(generate:unit -> 'a, initialPoolCount) = AgentObjectPool<'a>(Func<'a>(generate), initialPoolCount)

    /// Puts an item into the pool
    member this.PutObject(item) =  agent.Post(Put(item))
    /// Gets an item from the pool or if there are none present use the generator
    member this.GetObject() = agent.PostAndAsyncReply(Get) |> Async.StartAsTask
    /// Gets the current count of items in the pool
    member this.GetAllocationObject() = agent.PostAndAsyncReply(GetCount) |> Async.StartAsTask
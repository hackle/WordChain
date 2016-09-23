module WordChainTests
open Xunit
open WordChainKata

let shouldCancel () = false
let mutable chain:Option<string list> = None;
let chainSetter c = chain <- Some c
let chainGetter () = chain

[<Fact>]
let ``Chain of a word itself is itself`` () =
    let chainMaker = new ChainMaker([ "great" ], "great", "great", 255, shouldCancel, chainGetter, chainSetter)
    Assert.Equal<string list>([ "great" ], chainMaker.Make() )
    
[<Fact>]
let ``works correctly for small set 1`` () =
    let set = [ "cat"; "cow"; "pat"; "cot"; "cog"; "dog" ]
    let chainMaker = new ChainMaker(set, "cat", "dog", 255, shouldCancel, chainGetter, chainSetter)
    Assert.Equal<string list>([ "cat"; "cot"; "cog"; "dog" ], chainMaker.Make())
    
[<Fact>]
let ``works correctly for small set 2`` () =
    let set = [ "aft"; "fast"; "raft"; "fist"; "rafts"; "fists"; "rants"; "lists"; "chants"; "list"; "chant"; "lisp" ]
    let chainMaker = new ChainMaker(set, "aft", "rants", 255, shouldCancel, chainGetter, chainSetter)
    Assert.Equal<string list>([ "aft"; "raft"; "rafts"; "rants" ], chainMaker.Make())

[<Fact>]
let ``works correctly for small set 2, reversed`` () =
    let set = [ "aft"; "fast"; "raft"; "fist"; "rafts"; "fists"; "rants"; "lists"; "chants"; "list"; "chant"; "lisp" ]
    let chainMaker = new ChainMaker(set, "rants", "aft", 255, shouldCancel, chainGetter, chainSetter)
    Assert.Equal<string list>([ "rants"; "rafts"; "raft"; "aft" ], chainMaker.Make())
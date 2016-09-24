module WordChainTests
open Xunit
open WordChainKata

let shouldCancel () = false
let mutable chain:Option<string list> = None;
let chainSetter c = chain <- Some c
let chainGetter () = chain

[<Fact>]
let ``Chain of a word itself is itself`` () =
    chain <- None
    let chain = makeChain [ "great" ] "great" "great" 255 shouldCancel chainGetter chainSetter
    Assert.Equal<string list>([ "great" ], chain )
    
[<Fact>]
let ``works correctly for small set 1`` () =
    chain <- None
    let set = [ "cat"; "cow"; "pat"; "cot"; "cog"; "dog" ]
    let chain = makeChain set "cat" "dog" 255 shouldCancel chainGetter chainSetter
    Assert.Equal<string list>([ "cat"; "cot"; "cog"; "dog" ], chain)
    
[<Fact>]
let ``works correctly for small set 2`` () =
    chain <- None
    let set = [ "aft"; "fast"; "raft"; "fist"; "rafts"; "fists"; "rants"; "lists"; "chants"; "list"; "chant"; "lisp" ]
    let chain = makeChain set "aft" "rants" 255 shouldCancel chainGetter chainSetter
    Assert.Equal<string list>([ "aft"; "raft"; "rafts"; "rants" ], chain)

[<Fact>]
let ``works correctly for small set 2, reversed`` () =
    chain <- None
    let set = [ "aft"; "fast"; "raft"; "fist"; "rafts"; "fists"; "rants"; "lists"; "chants"; "list"; "chant"; "lisp" ]
    let chain = makeChain set "rants" "aft" 255 shouldCancel chainGetter chainSetter
    Assert.Equal<string list>([ "rants"; "rafts"; "raft"; "aft" ], chain)
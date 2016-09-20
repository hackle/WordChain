module WordChainTests
open Xunit
open WordChainKata

[<Fact>]
let ``Chain of a word itself is itself`` () =
    let chain = makeChain [ "great" ] "great" "great" 255
    Assert.Equal<string list>([ "great" ], chain)
    
[<Fact>]
let ``works correctly for small set 1`` () =
    let set = [ "cat"; "cow"; "pat"; "cot"; "cog"; "dog" ]
    let chain = makeChain set "cat" "dog" 255
    Assert.Equal<string list>([ "cat"; "cot"; "cog"; "dog" ], chain)
    
[<Fact>]
let ``works correctly for small set 2`` () =
    let set = [ "aft"; "fast"; "raft"; "fist"; "rafts"; "fists"; "rants"; "lists"; "chants"; "list"; "chant"; "lisp" ]
    let chain = makeChain set "aft" "rants" 255
    Assert.Equal<string list>([ "aft"; "raft"; "rafts"; "rants" ], chain)

[<Fact>]
let ``works correctly for small set 2, reversed`` () =
    let set = [ "aft"; "fast"; "raft"; "fist"; "rafts"; "fists"; "rants"; "lists"; "chants"; "list"; "chant"; "lisp" ]
    let chain = makeChain set "rants" "aft" 255
    Assert.Equal<string list>([ "rants"; "rafts"; "raft"; "aft" ], chain)
module WordChainTests
open Xunit
open WordChainKata

[<Fact>]
let ``Chain of a word itself is itself`` () =
    let chain = makeChain [ "great" ] "great" "great"
    Assert.Equal<string list>([ "great" ], chain)

//[<Theory>]
//[<inlinedata([| "great"; "grate" |], "great", "grate")>]
//let ``chains correctly`` (words:string array) (fromword:string) (toword:string) =
//    let chain = makeChain 

[<Fact>]
let ``works correctly for small set`` () =
    let set = [ "cat"; "cow"; "pat"; "cot"; "cog"; "dog" ]
    let chain = makeChain set "cat" "dog"
    Assert.Equal<string list>([ "cat"; "cot"; "cog"; "dog" ], chain)
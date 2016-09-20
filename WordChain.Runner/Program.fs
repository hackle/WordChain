// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open WordChainKata

[<EntryPoint>]
let main argv = 
    printfn "From word:"
    let fromWord = System.Console.ReadLine()
    printfn "To word:"
    let toWord = System.Console.ReadLine()
    printfn "Searching for chain from %s to %s" fromWord toWord

    getChainForWords fromWord toWord 15

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

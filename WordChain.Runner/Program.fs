// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open WordChainKata

[<EntryPoint>]
let main argv =
    let rec makeChain () =
        let cancelSource = new System.Threading.CancellationTokenSource()
        let handler (args: System.ConsoleCancelEventArgs) = 
            args.Cancel <- true
            cancelSource.Cancel()
        System.Console.CancelKeyPress.Add handler

        printfn "From word:"
        let fromWord = System.Console.ReadLine()

        printfn "To word:"
        let toWord = System.Console.ReadLine()

        printfn "Max chain length:"
        let length = int (System.Console.ReadLine())

        printfn "Searching for chain from %s to %s" fromWord toWord

        getChainForWords fromWord toWord length cancelSource
        
        printfn "Press t to try again"
        if System.Console.ReadKey(true).KeyChar = 't' then
            makeChain ()
        else ()

    try
        makeChain ()
    with
    | _ -> ()

    0 // return an integer exit code

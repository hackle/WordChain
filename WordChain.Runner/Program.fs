// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open WordChainKata

let makeChainWithFile 
    localFileName
    (fromWord:string) 
    (toWord:string) 
    sizeLimit 
    (cancelSource: System.Threading.CancellationTokenSource) =

    let regexJustLetters = new System.Text.RegularExpressions.Regex(@"^[a-zA-Z]+$", System.Text.RegularExpressions.RegexOptions.Compiled)
    // assuming scope; in fact it should work via temporarily stepping out of scope (then back in)
    let minLength = -2 + min fromWord.Length toWord.Length
    let maxLength = 2 + max fromWord.Length toWord.Length
    let set =
        localFileName
        |> System.IO.File.ReadAllLines
        |> List.ofSeq
        |> List.filter (fun s -> regexJustLetters.IsMatch s)
        |> List.filter (fun s -> s.Length >= minLength && s.Length <= maxLength)
        |> List.map (fun w -> w.ToLower())
        |> List.distinct
        
    let mutable sharedChain:Option<string list> = None
    let refOfSharedChain = ref sharedChain
    let shouldCancel () = cancelSource.IsCancellationRequested
    let forward = async {
            let maker = new ChainMaker(set, fromWord, toWord, sizeLimit, shouldCancel, refOfSharedChain)
            return maker.Make()
        }
    let backward = async {
            let maker = new ChainMaker(set, toWord, fromWord, sizeLimit, shouldCancel, refOfSharedChain)
            return maker.Make() |> List.rev
        }

    let allTasks = 
        [ forward; backward ]
        |> List.map Async.StartAsTask

    let firstFinished =
        allTasks
        |> (fun tasks -> System.Threading.Tasks.Task.WhenAny(tasks).Result)

    allTasks
    |> System.Threading.Tasks.Task.WhenAll
    |> (fun t -> t.Wait())
    |> ignore

    let result =
        match !refOfSharedChain with
        | None -> []
        | Some c -> c

    printfn "The best chain so far is %i words long %A" (result |> List.length) result

[<EntryPoint>]
let main argv =
    printfn "File with list of words:"
    let filename = System.Console.ReadLine()

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

        makeChainWithFile filename fromWord toWord length cancelSource
        
        printfn "Press t to try again"
        if System.Console.ReadKey(true).KeyChar = 't' then
            makeChain ()
        else ()

    try
        makeChain ()
    with
    | e -> printfn "Error %A" e

    0 // return an integer exit code

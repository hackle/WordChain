module WordChainKata

let areDifferentByOne (chars1:char list) (chars2:char list) =
    let lenDiff = 
        (List.length chars1) - (List.length chars2)
        |> abs

    if lenDiff <> 1 then false
    else
        let isOffByOne c1 c2 =
            List.except c1 c2 
            |> (fun e -> List.except e c2)
            |> (=) c1

        (isOffByOne chars1 chars2) || (isOffByOne chars2 chars1)

let getDistance (c1:char seq) (c2:char seq) =
    if areDifferentByOne (List.ofSeq c1) (List.ofSeq c2) then
        1
    else
        let lengthDiff =
            (c1 |> Seq.length) - (c2 |> Seq.length)
            |> abs

        Seq.map2 (fun c1 c2 -> c1, c2) c1 c2
        |> Seq.filter (fun pair ->
                        let (c1, c2) = pair
                        c1 <> c2)
        |> Seq.length
        |> (+) lengthDiff

let areNeighbors (word1:string) (word2:string) =
    let chars1 = word1.ToCharArray() |> List.ofSeq
    let chars2 = word2.ToCharArray() |> List.ofSeq
    getDistance chars1 chars2 = 1

let isChainComplete (chain:string list) (finalWord:string) =
    (chain |> List.head) = finalWord

type ChainState = { Better: bool; Valid: bool }

type ChainMaker (set,
                    fromWord,
                    toWord,
                    sizeLimit,
                    cancelToken:System.Threading.CancellationToken,
                    ?sharedBestChain: Option<string list>) =
                    
    let mutable bestChain:Option<string list> = defaultArg sharedBestChain None
    let rec search 
            (fromChain:string list)
            (t:string): unit =

        let f = fromChain |> List.head

        let bestChainLen =
            match bestChain with
            | None -> 0
            | Some bc -> List.length bc

        printfn "from %A to %A best %i current %i, %A" f t bestChainLen (fromChain|>List.length) fromChain
        
        let isComplete = isChainComplete fromChain t
        let isWithinSizeLimit = List.length fromChain < sizeLimit
        let chainState =
            match bestChain with
            | None -> { Better = isComplete; Valid = not isComplete && isWithinSizeLimit }
            | Some bc -> 
                let isBetter = 
                    isComplete && 
                    (List.length fromChain) < (List.length bc)

                let stillHopeful = 
                    (List.length bc) - (List.length fromChain) > (getDistance f t)

                let isValid = 
                    not isComplete && 
                    stillHopeful &&
                    isWithinSizeLimit
                { Better = isBetter; Valid = isValid }

        match chainState.Better, chainState.Valid with
        | true, _ ->
            bestChain <- Some fromChain
            ()
        | false, false -> ()
        | false, true ->
            if cancelToken.IsCancellationRequested then ()
            else
                set
                |> List.filter (fun w -> not (List.contains w fromChain) && (areNeighbors f w))
                |> List.except [ f ]
                |> List.sortBy (fun w -> getDistance w t)
                |> List.iter (fun w -> search (w :: fromChain) t)

    member this.Make () =
        search [ fromWord ] toWord

        match bestChain with
        | None -> []
        | Some c -> List.rev c

let getChainForWords 
    (fromWord:string) 
    (toWord:string) 
    sizeLimit 
    (cancelSource: System.Threading.CancellationTokenSource) =

    let regexJustLetters = new System.Text.RegularExpressions.Regex(@"^[a-zA-Z]+$", System.Text.RegularExpressions.RegexOptions.Compiled)
    // assuming scope; in fact it should work via temporarily stepping out of scope (then back in)
    let minLength = -2 + min fromWord.Length toWord.Length
    let maxLength = 2 + max fromWord.Length toWord.Length
    let set =
        "E:\work\F#\wordlist.txt"
        |> System.IO.File.ReadAllLines
        |> List.ofSeq
        |> List.filter (fun s -> regexJustLetters.IsMatch s)
        |> List.filter (fun s -> s.Length >= minLength && s.Length <= maxLength)
        |> List.map (fun w -> w.ToLower())
        |> List.distinct
        
    let forward = async {
            let maker = new ChainMaker(set, fromWord, toWord, sizeLimit, (cancelSource.Token))
            return maker.Make()
        }
    let backward = async {
            let maker = new ChainMaker(set, toWord, fromWord, sizeLimit, (cancelSource.Token))
            return maker.Make() |> List.rev
        }

    let allTasks = 
        [ forward; backward ]
        |> List.map Async.StartAsTask

    let firstFinished =
        allTasks
        |> (fun tasks -> System.Threading.Tasks.Task.WhenAny(tasks).Result)

    cancelSource.Cancel()

    allTasks
    |> System.Threading.Tasks.Task.WhenAll
    |> (fun t -> t.Wait())
    |> ignore

    printfn "The chain is %A" firstFinished.Result
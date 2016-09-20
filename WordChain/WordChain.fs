module WordChainKata

type Chain = { Word: string; Distance: int; Neighbors: Chain list }

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

type ChainState = { Better: bool; Valid: bool }
let makeChain set fromWord toWord sizeLimit =
    let mutable bestChain:Option<string list> = None
    let isChainComplete (chain:string list) (finalWord:string) =
        (chain |> List.head) = finalWord
    
    let rec search 
            (chain:string list) 
            (f:string) 
            (t:string): string list list =
        let bestLength =
            match bestChain with
            | None -> 0
            | Some bc -> List.length bc
        let cancellationSource = new System.Threading.CancellationTokenSource()
        printfn "from %A to %A best %i current %i, %A" f t bestLength (chain|>List.length) chain

        let isComplete = isChainComplete chain t
        let isWithinSizeLimit = List.length chain < sizeLimit
        let chainState =
            match bestChain with
            | None -> { Better = isComplete; Valid = not isComplete && isWithinSizeLimit }
            | Some bc -> 
                let isBetter = isComplete && (List.length chain) < (List.length bc)
                let stillRoomToGrow = (List.length bc) - (List.length chain) > (getDistance f t)
                let isValid = 
                    not isComplete && 
                    stillRoomToGrow &&
                    isWithinSizeLimit
                { Better = isBetter; Valid = isValid }
                        
        if chainState.Better then
            bestChain <- Some chain
            cancellationSource.Cancel()
            []
        // not better and invalid
        elif not chainState.Valid then []
        // not better but still valid
        else
            set
            |> List.filter (fun w -> not (List.contains w chain) && (areNeighbors f w))
            |> List.except [ f ]
            |> List.sortBy (fun w -> getDistance w t)
            |> List.map (fun w -> 
                            async { 
                                return search (w :: chain) w t
                            })
            |> Async.Parallel
            |> (fun t -> Async.RunSynchronously (t,cancellationToken=cancellationSource.Token))
            |> List.concat
            |> List.filter (fun l -> List.length l > 0) // get rid of empty lists

    search [ fromWord ] fromWord toWord |> ignore
    match bestChain with
    | None -> []
    | Some c -> List.rev c

let getChainForWords (fromWord:string) (toWord:string) sizeLimit =
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

    makeChain set fromWord toWord sizeLimit
    |> printfn "The chain is %A"
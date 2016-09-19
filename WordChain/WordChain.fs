module WordChainKata

type Chain = { Word: string; Distance: int; Neighbors: Chain list }

let getDistance (c1:char seq) (c2:char seq) =
    let lengthDiff =
        (c1 |> Seq.length) - (c2 |> Seq.length)
        |> abs

    Seq.map2 (fun c1 c2 -> c1, c2) c1 c2
    |> Seq.filter (fun pair ->
                    let (c1, c2) = pair
                    c1 <> c2)
    |> Seq.length
    |> (+) lengthDiff
    
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

let areNeighbors (word1:string) (word2:string) =
    let chars1 = word1.ToCharArray() |> List.ofSeq
    let chars2 = word2.ToCharArray() |> List.ofSeq
    (1 = getDistance chars1 chars2) || (areDifferentByOne chars1 chars2)

let processNeighbors withinMind set word =
    set
    |> List.except [ word ]
    |> List.filter (fun w -> 1 = getDistance word w)
    |> List.sortBy (fun w -> getDistance w withinMind)  //with this word in mind
    |> List.map (fun w -> { Word = w; 
                            Distance = 1; 
                            Neighbors = [] })

let processSet withinMind set =
    set 
    |> List.map (fun w -> { Word = w;
                            Distance = 0;  
                            Neighbors = processNeighbors withinMind set w })

type ChainState = { Better: bool; Valid: bool }
let makeChain set fromWord toWord =
    let mutable bestChain:Option<string list> = None
    let isChainComplete (chain:string list) (finalWord:string) =
        (chain |> List.head) = finalWord
    
    let rec search (chain:string list) (f:string) (t:string) : string list list =
        let bestLength =
            match bestChain with
            | None -> 0
            | Some bc -> List.length bc
        printfn "from %A to %A best %i current %i, %A" f t bestLength (chain|>List.length) chain

        let isComplete = isChainComplete chain t
        let chainState =
            match bestChain with
            | None -> { Better = isComplete; Valid = not isComplete }
            | Some bc -> 
                let isBetter = isComplete && (List.length chain) < (List.length bc)
                let stillRoomToGrow = (List.length bc) - (List.length chain) > (getDistance f t)
                let isValid = not isComplete && stillRoomToGrow
                { Better = isBetter; Valid = isValid }
                        
        if chainState.Better then
            bestChain <- Some chain
            []
        // not better and invalid
        elif not chainState.Valid then []
        // not better but still valid
        elif List.contains f set then
            set
            |> List.filter (fun w -> (areNeighbors f w) && not (List.contains w chain))
            |> List.except [ f ]
            |> List.sortBy (fun w -> getDistance w t)
            |> List.map (fun w -> search (w :: chain) w t)
            |> List.filter (fun l -> List.length l > 0) // get rid of empty lists
            |> List.concat
        else []

    search [ fromWord ] fromWord toWord |> ignore
    match bestChain with
    | None -> []
    | Some c -> List.rev c

let getChainForWords (fromWord:string) (toWord:string) =
    let regexJustLetters = new System.Text.RegularExpressions.Regex(@"^[a-zA-Z]+$", System.Text.RegularExpressions.RegexOptions.Compiled)
    let set =
        async {
            let client = new System.Net.WebClient()
            let! html = client.AsyncDownloadString(new System.Uri("http://codekata.com/data/wordlist.txt"))
            return html.Split([| "\r"; "\n" |], System.StringSplitOptions.RemoveEmptyEntries)
        }
        |> Async.RunSynchronously
        |> Seq.filter (fun s -> regexJustLetters.IsMatch s)
//        |> Seq.filter (fun s -> toWord.Length > (getDistance s toWord) || toWord.Length > (getDistance s fromWord))
        |> List.ofSeq
        |> List.map (fun w -> w.ToLower())
        |> List.distinct

    makeChain set fromWord toWord
    |> printfn "The chain is %A"
module WordChainKata

type Chain = { Word: string; Distance: int; Neighbors: Chain list }

let getDistance (word1:string) (word2:string) =
    Seq.map2 (fun c1 c2 -> c1, c2) (word1.ToCharArray()) (word2.ToCharArray())
    |> Seq.filter (fun pair ->
                    let (c1, c2) = pair
                    c1 <> c2)
    |> Seq.length
    
let processNeighbors set word =
    set
    |> List.except [ word ]
    |> List.filter (fun w -> 1 = getDistance word w)
    |> List.map (fun w -> { Word = w; 
                            Distance = 1; 
                            Neighbors = [] })

let processSet set =
    set 
    |> List.map (fun w -> { Word = w;
                            Distance = 0;  
                            Neighbors = processNeighbors set w })
    
let makeChain set fromWord toWord =
    let processedSet = processSet set
    let mutable bestChain:Option<string list> = None
    let isChainComplete (chain:string list) (finalWord:string) =
        (chain |> List.head) = finalWord
    
    let rec search (chain:string list) (f:string) (t:string) : string list list =
        printfn "current chain %A f %A t %A" chain f t
        let isBetterChain =
            match bestChain with
            | None -> isChainComplete chain t
            | Some bc -> (isChainComplete chain t) && (List.length chain) < (List.length bc)

        if isBetterChain then
            bestChain <- Some chain
            []
        // complete, but not better
        elif (isChainComplete chain t) then []
        // incomplete
        else                
            let currentWords = 
                processedSet 
                |> List.filter (fun x -> x.Word = f)

            match currentWords with
            | fromNode::_ ->
                fromNode.Neighbors
                |> List.filter (fun w -> not (List.contains w.Word chain))
                |> List.map (fun w -> search (w.Word :: chain) w.Word t)
                |> List.filter (fun l -> List.length l > 0) // get rid of empty lists
                |> List.concat            
            | [] -> []

    search [ fromWord ] fromWord toWord |> ignore
    bestChain

let getChainForWords (fromWord:string) (toWord:string) =
    let set =
        async {
            let client = new System.Net.WebClient()
            let! html = client.AsyncDownloadString(new System.Uri("http://codekata.com/data/wordlist.txt"))
            return html.Split([| "\r"; "\n" |], System.StringSplitOptions.RemoveEmptyEntries)
        }
        |> Async.RunSynchronously
        |> Seq.filter (fun s -> s.Length = fromWord.Length)
        |> Seq.filter (fun s -> toWord.Length > (getDistance s toWord) || toWord.Length > (getDistance s fromWord))
        |> List.ofSeq
        |> List.map (fun w -> w.ToLower())
        |> List.distinct

    let chain = makeChain set fromWord toWord
    match chain with
    | None -> printfn "No match was found"
    | Some c -> printfn "The chain is %A" (c |> List.rev)
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
    |> List.map (fun w -> { Word = w; 
                            Distance = getDistance word w; 
                            Neighbors = [] })

let processSet set =
    set 
    |> List.map (fun w -> { Word = w;
                            Distance = 0;  
                            Neighbors = processNeighbors set w })
    
let makeChain set fromWord toWord =
    let processedSet = processSet set
    let mutable chain:string list list = []
    
    let rec make currentChain f t =
        if f = t then 
            chain <- currentChain :: chain
            []
        else
            match processedSet |> List.filter (fun x -> x.Word = f) with
            | [] -> []
            | fromSet::_ ->
                fromSet.Neighbors
                |> List.filter (fun w -> 1 = (getDistance fromSet.Word w.Word) && not (List.contains w.Word currentChain))
                |> List.map (fun w -> { Word = w.Word;
                                        Distance = getDistance fromSet.Word w.Word 
                                        Neighbors = make (w.Word :: currentChain) w.Word t })
    make [ fromWord ] fromWord toWord |> ignore
    chain

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
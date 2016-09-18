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
    let mutable processed:string list = []
    let mutable chain:Option<string list> = None
    
    let isWordProcessed w = 
        List.contains w processed

    let rec make currentChain f t =
        let alreadyProcessed = isWordProcessed f
        if not alreadyProcessed then        
            processed <- f :: processed

        if f = t then chain <- Some currentChain

        if alreadyProcessed || (Option.isSome chain) then []
        else          
            match processedSet |> List.filter (fun x -> x.Word = f) with
            | [] -> []
            | fromSet::_ ->
                fromSet.Neighbors
                |> List.filter (fun w -> 1 = (getDistance fromSet.Word w.Word) && not (isWordProcessed w.Word))
                |> List.map (fun w -> { Word = w.Word;
                                        Distance = getDistance fromSet.Word w.Word 
                                        Neighbors = make (w.Word :: currentChain) w.Word t })
    make [ fromWord ] fromWord toWord |> ignore
    chain
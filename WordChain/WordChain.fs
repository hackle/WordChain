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

let getDistance (chars1:char seq) (chars2:char seq) =
    if areDifferentByOne (List.ofSeq chars1) (List.ofSeq chars2) then
        1
    else
        let lengthDiff =
            (chars1 |> Seq.length) - (chars2 |> Seq.length)
            |> abs

        Seq.map2 (fun c1 c2 -> c1, c2) chars1 chars2
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
                    shouldCancel: unit -> bool,
                    getBestChain: unit -> Option<string list>,
                    setBestChain: string list -> unit) =

    let rec search 
            (currentChain:string list): unit =

        if shouldCancel() then ()
        else
            let headWord = currentChain |> List.head
            let bestChain = getBestChain()

            let bestChainLen =
                match bestChain with
                | None -> 0
                | Some bc -> List.length bc

            printfn "from %A to %A best %i current %i, %A" headWord toWord bestChainLen (currentChain|>List.length) currentChain
        
            let isComplete = isChainComplete currentChain toWord
            let isWithinSizeLimit = List.length currentChain < sizeLimit
            let chainState =
                match bestChain with
                | None -> { Better = isComplete; Valid = not isComplete && isWithinSizeLimit }
                | Some bc -> 
                    let isBetter = 
                        isComplete && 
                        (List.length currentChain) < (List.length bc)

                    let stillHopeful = 
                        (List.length bc) - (List.length currentChain) > (getDistance headWord toWord)

                    let isValid = 
                        not isComplete && 
                        stillHopeful &&
                        isWithinSizeLimit
                    { Better = isBetter; Valid = isValid }

            match chainState.Better, chainState.Valid with
            | true, _ ->
                setBestChain(currentChain)
                ()
            | false, false -> ()
            | false, true ->
                set
                |> List.filter (fun w -> not (List.contains w currentChain) && (areNeighbors headWord w))
                |> List.except [ headWord ]
                |> List.sortBy (fun w -> getDistance w toWord)
                |> List.iter (fun w -> search (w :: currentChain))

    member this.Make () =
        search [ fromWord ]

        match getBestChain() with
        | None -> []
        | Some c -> List.rev c
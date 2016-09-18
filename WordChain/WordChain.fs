module WordChainKata

type Neighbor = { Word: string; Distance: int }
type WordWithNeigbors = { Word: string; Neighbors: Neighbor list }

let wordsDistance (word1:string) (word2:string) =
    let word2Chars = word2.ToCharArray()

    word1.ToCharArray()
    |> Seq.except word2Chars
    |> Seq.length

let processNeighbors set word =
    set
    |> List.except [ word ]
    |> List.map (fun w -> { Word = w; Distance = wordsDistance word w })

let processSet set =
    set 
    |> List.map (fun w -> { Word = w; Neighbors = processNeighbors set w })

let makeChain set fromWord toWord =
    let processed = process set

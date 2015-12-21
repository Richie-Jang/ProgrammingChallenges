open System

let getColAndRow (ins: string) =
    let ls = ins.Split(' ')
    (int ls.[1], int ls.[0])

let moves =
    [(-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1)]

let getPossibles r1 c1 r c =
    let nmoves = 
        moves
        |> List.map (fun d -> (fst d) + r1, (snd d) + c1)
        |> List.filter 
            (fun d -> 
                let a1 = (fst d)
                let a2 = (snd d)
                (0 <= a1) && (a1 <= r-1) && (0 <= a2) && (a2 <= c-1)
            )
    nmoves
     

let checkMap (map: char[,]) =
    let r = map |> Array2D.length1
    let c = map |> Array2D.length2
    let result = Array2D.zeroCreate<string> r c
    for a1 = 0 to r-1 do
        for b1 = 0 to c-1 do
            let d = map.[a1,b1]
            match d with
            | '*' -> result.[a1,b1] <- "*"
            | _   ->
                let ps = getPossibles a1 b1 r c
                let pps = 
                    ps |> List.countBy (fun t -> map.[(fst t), (snd t)] = '*')
                let finds =
                    pps
                    |> List.tryFind (fun t -> (fst t))
                match finds with
                | Some (nc) -> result.[a1,b1] <- (snd nc).ToString()
                | None -> result.[a1,b1] <- "0"
    result

let fieldCount = ref 0

let solve (map: char[,]) =
    incr fieldCount
    printfn "Field #%d" !fieldCount
    let rmap = checkMap map
    for i = 0 to (rmap |> Array2D.length1)-1 do
        for j = 0 to (rmap |> Array2D.length2)-1 do
            printf "%s" rmap.[i,j]
        printfn ""
    printfn ""

let mutable breaking = false
while (not breaking) do
    let col,row = getColAndRow (Console.ReadLine())
    if col = 0 && row = 0 then breaking <- true
    else
        let map = Array2D.zeroCreate row col
        for i = 0 to row-1 do
            let input = Console.ReadLine()
            input |> Seq.iteri
                (fun index k -> map.[i,index] <- k)
        solve map

Console.ReadLine() |> ignore

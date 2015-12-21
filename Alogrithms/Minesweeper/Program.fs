open System

let getColAndRow (ins: string) =
    let ls = ins.Split(' ')
    (int ls.[0], int ls.[1])

let solve (map: char[,]) =
    printfn "%A" map
    

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

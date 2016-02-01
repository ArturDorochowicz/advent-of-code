open System
open System.IO

let readInput =
  File.ReadAllText "day3.input"

let getPositions moves =
  let nextPosition (x, y) move =
    match move with
    | '^' -> x, y+1
    | 'v' -> x, y-1
    | '>' -> x+1, y
    | '<' -> x-1, y
    | _ -> failwith "Unknown move."
  seq {
    let mutable p = 0, 0
    yield p
    for m in moves do
      p <- nextPosition p m
      yield p
  }

let part1 =
  readInput
  |> getPositions
  |> Seq.distinct
  |> Seq.length

let part2 =
  readInput
  |> Seq.mapi (fun i x -> i % 2, x)
  |> Seq.groupBy fst
  |> Seq.collect (fun (_, xs) -> xs |> Seq.map snd |> getPositions)
  |> Seq.distinct
  |> Seq.length

part1 |> printfn "%d"   // 2565
part2 |> printfn "%d"   // 2639

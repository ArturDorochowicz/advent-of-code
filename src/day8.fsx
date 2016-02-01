open System
open System.IO

let readInput () =
  File.ReadAllLines "day8.input"

module Part1 =

  let rec memoryLength = function
  | '\\' :: '\\' :: tail
  | '\\' :: '"' :: tail
  | '\\' :: 'x' ::  _ ::  _ :: tail -> 1 + (memoryLength tail)
  | head::tail -> 1 + (memoryLength tail)
  | [] -> 0 - "\"\"".Length

  let value () =
    readInput ()
    |> Seq.sumBy (fun x -> x.Length - (x |> List.ofSeq |> memoryLength))

module Part2 =

  let rec length = function
  | '\\' :: tail
  | '"' :: tail -> 2 + length tail
  | head :: tail -> 1 + length tail
  | [] -> "\"\"".Length

  let value () =
    readInput ()
    |> Seq.sumBy (fun x -> (x |> List.ofSeq |> length) - x.Length)

Part1.value () |> printfn "%d"   // 1350
Part2.value () |> printfn "%d"   // 2085

open System.IO

let readInput =
  File.ReadAllText "day1.input"

let toValue c =
  match c with
  | '(' -> 1
  | ')' -> -1
  | _ -> failwith "Invalid character"

let part1 =
  readInput
  |> Seq.map toValue
  |> Seq.sum

let part2 =
  readInput
  |> Seq.scan (fun state c -> state + toValue c) 0
  |> Seq.findIndex ((=) -1)

part1 |> printfn "%d"   // 138
part2 |> printfn "%d"   // 1771

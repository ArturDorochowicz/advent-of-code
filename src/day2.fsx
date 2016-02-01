open System
open System.IO
open System.Globalization

let readInput =
  File.ReadAllLines "day2.input"

let parseLine (line:string) =
  let dims =
    line.Split ([|'x'|], 3)
    |> Array.map int
  dims.[0], dims.[1], dims.[2]

let surface (l,w,h) =
  let sm = Array.sort [|l;w;h|]
  2 * l * w + 2 * l * h + 2 * w * h + sm.[0] * sm.[1]

let ribbonLength (l, w, h) =
  let sm = Array.sort [|l;w;h|]
  2 * sm.[0] + 2 * sm.[1] + l * w * h

let part1 =
  readInput
  |> Seq.map parseLine
  |> Seq.map surface
  |> Seq.sum

let part2 =
  readInput
  |> Seq.map parseLine
  |> Seq.map ribbonLength
  |> Seq.sum

part1 |> printfn "%d"   // 1606483
part2 |> printfn "%d"   // 3842356

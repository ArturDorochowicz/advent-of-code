open System
open System.IO

let readInput () =
  File.ReadAllLines "day8.input"

module Part1 =

  let memoryLength list =
    let rec length acc = function
      | '\\' :: '\\' :: tail
      | '\\' :: '"' :: tail
      | '\\' :: 'x' ::  _ ::  _ :: tail -> length (acc + 1) tail
      | head::tail -> length (acc + 1) tail
      | [] -> acc - "\"\"".Length
    length 0 list

  let value () =
    readInput ()
    |> Seq.sumBy (fun x -> x.Length - (x |> List.ofSeq |> memoryLength))

module Part2 =

  let encodedLength list =
    let rec length acc = function
      | '\\' :: tail
      | '"' :: tail -> length (acc + 2) tail
      | head :: tail -> length (acc + 1) tail
      | [] -> acc + "\"\"".Length
    length 0 list

  let value () =
    readInput ()
    |> Seq.sumBy (fun x -> (x |> List.ofSeq |> encodedLength) - x.Length)

Part1.value () |> printfn "%d"   // 1350
Part2.value () |> printfn "%d"   // 2085

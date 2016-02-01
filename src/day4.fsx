open System
open System.IO
open System.Text
open System.Security.Cryptography

let readInput () =
  //File.ReadAllText "day4.input"
  "ckczppom"

let getHash (md5:HashAlgorithm) key v =
  v |> (string >> Encoding.ASCII.GetBytes) |> Array.append key |> md5.ComputeHash

let part1 =
  use md5 =
   HashAlgorithm.Create "MD5"
  let input =
    readInput ()
    |> Encoding.ASCII.GetBytes

  Seq.initInfinite id
  |> Seq.map (getHash md5 input)
  |> Seq.findIndex (fun x -> x.[0] = 0uy && x.[1] = 0uy && x.[2] < 16uy)

let part2 =
  use md5 =
   HashAlgorithm.Create "MD5"
  let input =
    readInput ()
    |> Encoding.ASCII.GetBytes

  Seq.initInfinite id
  |> Seq.map (getHash md5 input)
  |> Seq.findIndex (fun x -> x.[0] = 0uy && x.[1] = 0uy && x.[2] = 0uy)


part1 |> printfn "%d"   // 117946
part2 |> printfn "%d"   // 3938038

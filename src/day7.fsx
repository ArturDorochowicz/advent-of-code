open System
open System.IO
open System.Collections.Generic

let memoize f =
  let cache = Dictionary<_, _>()
  fun x ->
      let mutable res = Unchecked.defaultof<_>
      let ok = cache.TryGetValue(x, &res)
      if ok then res
      else let res = f x
           cache.[x] <- res
           res

let readInput () =
  File.ReadAllLines "day7.input"

type Input =
  | Value of uint16
  | Wire of string

let parse input =
  try
    Input.Value (uint16 input)
  with
  | :? FormatException -> Wire input

type Gate =
  | And of inA:Input * inB:Input
  | Or of inA:Input * inB:Input
  | Not of ``in``:Input
  | LShift of inA:Input * inB:Input
  | RShift of inA:Input * inB:Input

type Source =
  | Gate of Gate
  | Signal of Input

let parseLine (line:string) =
  let parts = line.Split ' '
  match parts with
  | [|inA; "AND"; inB; _; out|]
    -> out, Gate (And (parse inA, parse inB))
  | [|inA; "OR"; inB; _; out|]
    -> out, Gate (Or (parse inA, parse inB))
  | [|"NOT"; inA; _; out|]
    -> out, Gate (Not (parse inA))
  | [|inA; "LSHIFT"; inB; _; out|]
    -> out, Gate (LShift (parse inA, parse inB))
  | [|inA; "RSHIFT"; inB; _; out|]
    -> out, Gate (RShift (parse inA, parse inB))
  | [|inA; "->"; out|]
    -> out, Signal (parse inA)
  | _
    -> failwith "WTF?!"

let eval (elements:IDictionary<_,_>) name =
  let rec e =
    memoize
      (fun name ->
        let evalInput =
          function
          | Value v -> v
          | Wire w -> e w
        let evalGate =
          function
          | And (a, b) -> evalInput a &&& evalInput b
          | Or (a, b) -> evalInput a ||| evalInput b
          | Not a -> ~~~(evalInput a)
          | LShift (a, b) -> evalInput a <<< int (evalInput b)
          | RShift (a, b) -> evalInput a >>> int (evalInput b)
        match elements.[name] with
        | Gate gate -> evalGate gate
        | Signal input -> evalInput input)
  e name


module Part1 =

  let elems =
    readInput ()
    |> Seq.map parseLine
    |> dict

  let value =
    eval elems "a"

module Part2 =

  let elems =
    readInput ()
    |> Seq.map parseLine
    |> Seq.filter (fst >> (<>) "b")
    |> Seq.append <| Seq.singleton ("b", Signal (Value 46065us))
    |> dict

  let value =
    eval elems "a"

Part1.value |> printfn "%d"   // 46065
Part2.value |> printfn "%d"   // 14134

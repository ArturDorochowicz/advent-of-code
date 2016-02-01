open System
open System.IO

let readInput () =
  File.ReadAllLines "day6.input"

type Coords =
  { X:int; Y:int}

type Action =
  | TurnOff of fromCoords : Coords * toCoords : Coords
  | TurnOn of fromCoords : Coords * toCoords : Coords
  | Toggle of fromCoords : Coords * toCoords : Coords

let parse (line:string) =
  let parts =
    line.Split [|' '|]
  let parseCoords (coordsString:string) =
    let arr =
      coordsString.Split ','
      |> Array.map int
    {X = arr.[0]; Y = arr.[1]}

  match parts with
  | [|"turn"; "off";fromString;_;toString|] -> TurnOff ((parseCoords fromString), (parseCoords toString))
  | [|"turn"; "on";fromString;_;toString|] -> TurnOn ((parseCoords fromString), (parseCoords toString))
  | [|"toggle";fromString;_;toString|] -> Toggle ((parseCoords fromString), (parseCoords toString))
  | _ -> invalidArg "line" ""

let act action s e (ar:int[,]) =
  for x = s.X to e.X do
    for y = s.Y to e.Y do
      ar.[x,y] <- action ar.[x,y]

let sum ar =
  let mutable on = 0
  let maxx = Array2D.length1 ar - 1
  let maxy = Array2D.length2 ar - 1
  for x = 0 to maxx do
    for y in 0..maxy do
      on <- on + ar.[x,y]
  on

module Part1 =
  let mutateArray action (ar:int[,]) =
    match action with
    | TurnOn (s, e) -> act (fun _ -> 1) s e ar
    | TurnOff (s, e) -> act (fun _ -> 0) s e ar
    | Toggle (s, e) -> act (fun v -> if v = 1 then 0 else 1) s e ar

  let run =
    readInput ()
    |> Seq.map parse
    |> Seq.fold
      (fun ar action ->
        mutateArray action ar
        ar)
      (Array2D.zeroCreate<int> 1000 1000)
    |> sum

module Part2 =

  let mutateArray action (ar:int[,]) =
    match action with
    | TurnOn (s, e) -> act (fun v -> v + 1) s e ar
    | TurnOff (s, e) -> act (fun v -> if v = 0 then 0 else v - 1) s e ar
    | Toggle (s, e) -> act (fun v -> v + 2) s e ar

  let run =
    readInput ()
    |> Seq.map parse
    |> Seq.fold
      (fun ar action ->
        mutateArray action ar
        ar)
      (Array2D.zeroCreate<int> 1000 1000)
    |> sum


Part1.run |> printfn "%d"  // 400410
Part2.run |> printfn "%d"  // 15343601

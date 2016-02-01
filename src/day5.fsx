open System
open System.IO

let readInput () =
  File.ReadAllLines "day5.input"

type State =
  {
    LastChar : char option
    Vowels : int
    Dup : bool
    Bad : bool
  }

let collectVowels vowels c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> vowels + 1
  | _ -> vowels

let collectDoubles dup lastChar c =
  match dup, lastChar with
  | true, _ -> true
  | false, None -> false
  | false, Some lastChar' -> c = lastChar'

let collectBadDoubles bad lastChar c =
  match bad, lastChar with
  | true, _ -> true
  | false, None -> false
  | false, Some lastChar' ->
    match lastChar', c with
    | 'a', 'b' | 'c', 'd' | 'p', 'q' | 'x', 'y' -> true
    | _ -> false

let isNice1 input =
  let state =
    ({ LastChar = None; Vowels = 0; Dup = false; Bad = false }, input)
    ||> Seq.fold (fun state c ->
      { state with
          LastChar = Some c
          Vowels = collectVowels state.Vowels c
          Dup = collectDoubles state.Dup state.LastChar c
          Bad = collectBadDoubles state.Bad state.LastChar c } )
  state.Bad = false
    && state.Dup =true
    && state.Vowels >= 3

let part1 =
  readInput ()
  |> Seq.map isNice1
  |> Seq.filter ((=) true)
  |> Seq.length

part1 |> printfn "%d"   // 255


type OneInBetweenState =
  | Start
  | One of first:char
  | Two of first:char * second:char
  | Fulfilled

let NextOneInBetweenState state c =
  match state, c with
  | Fulfilled, _ -> Fulfilled
  | Start, _ -> One c
  | One first, _ -> Two (first, c)
  | Two (first, second), c when first = c -> Fulfilled
  | Two (first, second), _ -> Two (second, c)

type DoublePairState =
  | Start
  | One of char
  | PairOfDifferent of char * Set<char*char>
  | PairOfSame of char * Set<char*char>
  | Fulfilled

let NextDoublePairState (state:DoublePairState) c =
  match state, c with
  | Fulfilled, _ -> Fulfilled
  | Start, _ -> One c
  | One prev, c when prev = c -> PairOfSame (c, Set.empty.Add(prev, c))
  | One prev, c -> PairOfDifferent (c, Set.empty.Add(prev, c))
  | PairOfSame (prev, pairs), c when prev = c -> PairOfDifferent(c, pairs)
  | PairOfSame (prev, pairs), c when Set.contains (prev,c) pairs -> Fulfilled
  | PairOfSame (prev, pairs), c -> PairOfDifferent (c, pairs.Add(prev,c))
  | PairOfDifferent (prev, pairs), c when Set.contains (prev,c) pairs -> Fulfilled
  | PairOfDifferent (prev, pairs), c when prev = c -> PairOfSame(c, pairs.Add(prev,c))
  | PairOfDifferent (prev, pairs), c -> PairOfDifferent(c, pairs.Add(prev,c))

let isNice2 input =
  let oneInBetween, doublePair =
    input
    |> Seq.fold (fun st c -> NextOneInBetweenState (fst st) c, NextDoublePairState (snd st) c) (OneInBetweenState.Start, Start)
  oneInBetween = OneInBetweenState.Fulfilled && doublePair = Fulfilled

let part2 =
  readInput ()
  |> Seq.map isNice2
  |> Seq.filter ((=) true)
  |> Seq.length

part2 |> printfn "%d"   // 55

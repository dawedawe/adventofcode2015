module Day16

[<Literal>]
let InputFile = "Day16Input.txt"

type Aunt =
    { Id: int
      Children: int Option
      Cats: int Option
      Samoyeds: int Option
      Pomeranians: int Option
      Akitas: int Option
      Vizslas: int Option
      Goldfish: int Option
      Trees: int Option
      Cars: int Option
      Perfumes: int Option }

let createAunt id =
    { Id = id
      Children = None
      Cats = None
      Samoyeds = None
      Pomeranians = None
      Akitas = None
      Vizslas = None
      Goldfish = None
      Trees = None
      Cars = None
      Perfumes = None }

let updateAunt aunt compound amount =
    match compound with
    | "children" -> { aunt with Children = Some amount }
    | "cats" -> { aunt with Cats = Some amount }
    | "samoyeds" -> { aunt with Samoyeds = Some amount }
    | "pomeranians" -> { aunt with Pomeranians = Some amount }
    | "akitas" -> { aunt with Akitas = Some amount }
    | "vizslas" -> { aunt with Vizslas = Some amount }
    | "goldfish" -> { aunt with Goldfish = Some amount }
    | "trees" -> { aunt with Trees = Some amount }
    | "cars" -> { aunt with Cars = Some amount }
    | "perfumes" -> { aunt with Perfumes = Some amount }
    | _ -> failwith "unknown compound"

let parse (s: string) =
    let a =
        s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun x -> x.Trim(':', ','))

    let id = a.[1] |> int
    let aunt = createAunt id

    a.[2..]
    |> Array.windowed 2
    |> Array.indexed
    |> Array.filter (fun (i, _) -> i % 2 = 0)
    |> Array.fold (fun a (_, w) -> updateAunt a w.[0] (int w.[1])) aunt

let tickerTape =
    { Id = -1
      Children = Some 3
      Cats = Some 7
      Samoyeds = Some 2
      Pomeranians = Some 3
      Akitas = Some 0
      Vizslas = Some 0
      Goldfish = Some 5
      Trees = Some 3
      Cars = Some 2
      Perfumes = Some 1 }

let isMatch (aunt: Aunt) =
    (aunt.Children = tickerTape.Children || aunt.Children.IsNone)
    && (aunt.Cats = tickerTape.Cats || aunt.Cats.IsNone)
    && (aunt.Samoyeds = tickerTape.Samoyeds || aunt.Samoyeds.IsNone)
    && (aunt.Pomeranians = tickerTape.Pomeranians || aunt.Pomeranians.IsNone)
    && (aunt.Akitas = tickerTape.Akitas || aunt.Akitas.IsNone)
    && (aunt.Vizslas = tickerTape.Vizslas || aunt.Vizslas.IsNone)
    && (aunt.Goldfish = tickerTape.Goldfish || aunt.Goldfish.IsNone)
    && (aunt.Trees = tickerTape.Trees || aunt.Trees.IsNone)
    && (aunt.Cars = tickerTape.Cars || aunt.Cars.IsNone)
    && (aunt.Perfumes = tickerTape.Perfumes || aunt.Perfumes.IsNone)

let day16 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map parse
    |> Array.filter isMatch

let predicateOrNone (compoundAmount: int option) predicate =
  Option.map predicate compoundAmount
  |> Option.defaultValue true

let isMatchPart2 (aunt: Aunt) =
    (aunt.Children = tickerTape.Children || aunt.Children.IsNone)
    && (predicateOrNone aunt.Cats (fun x -> x > tickerTape.Cats.Value))
    && (aunt.Samoyeds = tickerTape.Samoyeds || aunt.Samoyeds.IsNone)
    && (predicateOrNone aunt.Pomeranians (fun x -> x < tickerTape.Pomeranians.Value))
    && (aunt.Akitas = tickerTape.Akitas || aunt.Akitas.IsNone)
    && (aunt.Vizslas = tickerTape.Vizslas || aunt.Vizslas.IsNone)
    && (predicateOrNone aunt.Goldfish (fun x -> x < tickerTape.Goldfish.Value))
    && (predicateOrNone aunt.Trees (fun x -> x > tickerTape.Trees.Value))
    && (aunt.Cars = tickerTape.Cars || aunt.Cars.IsNone)
    && (aunt.Perfumes = tickerTape.Perfumes || aunt.Perfumes.IsNone)

let day16Part2 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map parse
    |> Array.filter isMatchPart2

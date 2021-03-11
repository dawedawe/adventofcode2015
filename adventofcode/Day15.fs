module Day15

[<Literal>]
let InputFile = "Day15Input.txt"

type Ingredient =
    { Name: string
      Capacity: int
      Durability: int
      Flavor: int
      Texture: int
      Calories: int }

let parse (s: string) =
    let words = s.Split(' ')

    { Name = words.[0].Trim(':')
      Capacity = int (words.[2].Trim(','))
      Durability = int (words.[4].Trim(','))
      Flavor = int (words.[6].Trim(','))
      Texture = int (words.[8].Trim(','))
      Calories = int (words.[10].Trim(',')) }

let calcScore (recipe: seq<Ingredient * int>) =
    let capacity =
        recipe
        |> Seq.sumBy (fun (i, c) -> c * i.Capacity)
        |> max 0

    let durability =
        recipe
        |> Seq.sumBy (fun (i, c) -> c * i.Durability)
        |> max 0

    let flavor =
        recipe
        |> Seq.sumBy (fun (i, c) -> c * i.Flavor)
        |> max 0

    let texture =
        recipe
        |> Seq.sumBy (fun (i, c) -> c * i.Texture)
        |> max 0

    let calories =
        recipe
        |> Seq.sumBy (fun (i, c) -> c * i.Calories)
        |> max 0

    (capacity * durability * flavor * texture, calories)

let calcAllRecipes (ingredients: Ingredient []) =
    seq {
        for i in 0 .. 100 do
            for j in 0 .. (100 - i) do
                for k in 0 .. (100 - i - j) do
                    for l in 0 .. (100 - i - j - k) do
                        let recipe =
                            [ (ingredients.[0], i)
                              (ingredients.[1], j)
                              (ingredients.[2], k)
                              (ingredients.[3], l) ]

                        yield (calcScore recipe)
    }

let day15 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map parse
    |> calcAllRecipes
    |> Seq.maxBy fst |> fst

let day15Part2 () =
    InputFile
    |> System.IO.File.ReadAllLines
    |> Array.map parse
    |> calcAllRecipes
    |> Seq.filter (fun (_, c) -> c = 500)
    |> Seq.maxBy fst |> fst
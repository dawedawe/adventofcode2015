module Day21

[<Literal>]
let InputFile = "Day21Input.txt"

type State =
    { Name: string
      HitPoints: int
      Damage: int
      Armor: int }

type Item =
    { Name: string
      Cost: int
      Damage: int
      Armor: int }

type Inventory = { Weapon: int; Armor: int; Rings: int }

let weapons =
    [ { Name = "Dagger"
        Cost = 8
        Damage = 4
        Armor = 0 }
      { Name = "Shortsword"
        Cost = 10
        Damage = 5
        Armor = 0 }
      { Name = "Warhammer"
        Cost = 25
        Damage = 6
        Armor = 0 }
      { Name = "Longsword"
        Cost = 40
        Damage = 7
        Armor = 0 }
      { Name = "Greataxe"
        Cost = 74
        Damage = 8
        Armor = 0 } ]

let armor =
    [ None
      { Name = "Leather"
        Cost = 13
        Damage = 0
        Armor = 1 }
      |> Some
      { Name = "Chainmail"
        Cost = 31
        Damage = 0
        Armor = 2 }
      |> Some
      { Name = "Splintmail"
        Cost = 53
        Damage = 0
        Armor = 3 }
      |> Some
      { Name = "Bandedmail"
        Cost = 75
        Damage = 0
        Armor = 4 }
      |> Some
      { Name = "Platemail"
        Cost = 102
        Damage = 0
        Armor = 5 }
      |> Some ]

let rings =
    [ { Name = "Damage +1"
        Cost = 25
        Damage = 1
        Armor = 0 }
      { Name = "Damage +2"
        Cost = 50
        Damage = 2
        Armor = 0 }
      { Name = "Damage +3"
        Cost = 100
        Damage = 3
        Armor = 0 }
      { Name = "Defense +1"
        Cost = 20
        Damage = 0
        Armor = 1 }
      { Name = "Defense +2"
        Cost = 40
        Damage = 0
        Armor = 2 }
      { Name = "Defense +3"
        Cost = 80
        Damage = 0
        Armor = 3 } ]

let ringsOrdered =
    seq {
        yield (None, None)

        for r in 0 .. rings.Length - 1 do
            yield (Some r, None)

        for r1 in 0 .. rings.Length - 1 do
            for r2 in 0 .. rings.Length - 1 do
                if r1 <> r2 then
                    yield (Some r1, Some r2)
    }
    |> Seq.sortBy
        (function
        | (None, None) -> 0
        | (Some r, None) -> rings.[r].Cost
        | (Some r1, Some r2) -> rings.[r1].Cost + rings.[r2].Cost
        | (None, Some _) -> failwith "bad state")
    |> Seq.toList


let parse (s: string []) =
    { Name = "Boss"
      HitPoints =
          s.[0]
              .Split(':', System.StringSplitOptions.RemoveEmptyEntries).[1]
          |> int
      Damage =
          s.[1]
              .Split(':', System.StringSplitOptions.RemoveEmptyEntries).[1]
          |> int
      Armor =
          s.[2]
              .Split(':', System.StringSplitOptions.RemoveEmptyEntries).[1]
          |> int }

let attack (attacker: State) (defender: State) =
    let damage = max (attacker.Damage - defender.Armor) 1

    { defender with
          HitPoints = defender.HitPoints - damage }

let rec game (attacker: State) (defender: State) =
    let defender' = attack attacker defender

    if defender'.HitPoints <= 0 then
        attacker.Name
    else
        game defender' attacker

let startingInventory = { Weapon = 0; Armor = 0; Rings = 0 }

let createPlayer (inventory: Inventory) =
    let armorP =
        armor.[inventory.Armor]
        |> Option.map (fun i -> i.Armor)
        |> Option.defaultValue 0

    let ring1Armor, ring1Damage =
        match fst ringsOrdered.[inventory.Rings] with
        | None -> 0, 0
        | Some r -> rings.[r].Armor, rings.[r].Damage

    let ring2Armor, ring2Damage =
        match snd ringsOrdered.[inventory.Rings] with
        | None -> 0, 0
        | Some r -> rings.[r].Armor, rings.[r].Damage


    { Name = "Player"
      HitPoints = 100
      Damage =
          weapons.[inventory.Weapon].Damage
          + ring1Damage
          + ring2Damage
      Armor = armorP + ring1Armor + ring2Armor }

let inventoryCost inventory =
    let wCost = weapons.[inventory.Weapon].Cost

    let aCost =
        armor.[inventory.Armor]
        |> Option.map (fun i -> i.Cost)
        |> Option.defaultValue 0

    let r1, r2 = ringsOrdered.[inventory.Rings]

    let r1Cost =
        r1
        |> Option.map (fun i -> rings.[i].Cost)
        |> Option.defaultValue 0

    let r2Cost =
        r2
        |> Option.map (fun i -> rings.[i].Cost)
        |> Option.defaultValue 0

    wCost + aCost + r1Cost + r2Cost

let improveInventory inventory =
    let nextWeapon =
        min (inventory.Weapon + 1) (weapons.Length - 1)

    let nextArmor =
        min (inventory.Armor + 1) (armor.Length - 1)

    let nextRings =
        min (inventory.Rings + 1) (ringsOrdered.Length - 1)

    let nextWeaponInventory = { inventory with Weapon = nextWeapon }
    let nextArmorInventory = { inventory with Armor = nextArmor }
    let nextRingsInventory = { inventory with Rings = nextRings }

    [ nextWeaponInventory
      nextArmorInventory
      nextRingsInventory ]
    |> List.filter (fun i -> i <> inventory)
    |> List.sortBy inventoryCost
    |> List.head

let inventories =
    seq {
        for w in 0 .. weapons.Length - 1 do
            for a in 0 .. armor.Length - 1 do
                for r in 0 .. ringsOrdered.Length - 1 do
                    yield { Weapon = w; Armor = a; Rings = r }
    }
    |> Seq.sortBy inventoryCost
    |> Seq.toList

let day21 () =
    let boss =
        InputFile |> System.IO.File.ReadAllLines |> parse

    seq {
        for i in inventories do
            let player = createPlayer i
            let winner = game player boss

            if winner = player.Name then
                yield inventoryCost i
    }
    |> Seq.head

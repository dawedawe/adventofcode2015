module Day22

open System.Collections.Generic

[<Literal>]
let InputFile = "Day22Input.txt"

type Spell =
    { Name: string
      Cost: int
      Damage: int
      Heal: int }

type Effect =
    { Name: string
      Cost: int
      Damage: int
      Heal: int
      Armor: int
      Mana: int
      Timer: int * int }

type SpellEffect =
    | Spell of Spell
    | Effect of Effect

type Player =
    { Name: string
      HitPoints: int
      Damage: int
      Armor: int
      Mana: int
      Spend: int }

type Boss =
    { Name: string
      HitPoints: int
      Damage: int }

let spells =
    [ { Name = "Magic Missile"
        Cost = 53
        Damage = 4
        Heal = 0 }
      { Name = "Drain"
        Cost = 73
        Damage = 2
        Heal = 2 } ]

let effects =
    [ { Name = "Shield"
        Cost = 113
        Damage = 0
        Heal = 0
        Armor = 7
        Mana = 0
        Timer = (6, 6) }
      { Name = "Poison"
        Cost = 173
        Damage = 3
        Heal = 0
        Armor = 0
        Mana = 0
        Timer = (6, 6) }
      { Name = "Recharge"
        Cost = 229
        Damage = 0
        Heal = 0
        Armor = 0
        Mana = 101
        Timer = (5, 5) } ]

let parse (s: string []) =
    { Name = "Boss"
      HitPoints =
          s.[0]
              .Split(':', System.StringSplitOptions.RemoveEmptyEntries).[1]
          |> int
      Damage =
          s.[1]
              .Split(':', System.StringSplitOptions.RemoveEmptyEntries).[1]
          |> int }

let applySpell (player: Player) (boss: Boss) (spell: Spell) =
    let boss' =
        { boss with
              HitPoints = boss.HitPoints - spell.Damage }

    let player' =
        { player with
              HitPoints = player.HitPoints + spell.Heal }

    (player', boss')

let applyEffectToPlayer (player: Player) (effect: Effect) =
    match effect.Name with
    | "Shield" ->
        if fst effect.Timer = snd effect.Timer then
            { player with Armor = player.Armor + 7 }
        else
            player
    | "Recharge" -> { player with Mana = player.Mana + 101 }
    | _ -> player

let applyEffectToBoss (boss: Boss) (effect: Effect) =
    match effect.Name with
    | "Poison" ->
        { boss with
              HitPoints = boss.HitPoints - 3 }
    | _ -> boss

let wearOffEffects (effects: Effect list) =
    effects
    |> List.map
        (fun e ->
            { e with
                  Timer = ((fst e.Timer) - 1, snd e.Timer) })
    |> List.filter (fun e -> fst e.Timer > 0)

let playRound (player: Player) (boss: Boss) (effects: Effect list) (spell: Spell option) =
    let mutable player' = player
    let mutable boss' = boss
    let mutable effects' = effects

    // player turn
    for e in effects' do
        player' <- applyEffectToPlayer player e
        boss' <- applyEffectToBoss boss e

    effects' <- wearOffEffects effects'

    match spell with
    | Some s ->
        let p, b = applySpell player' boss' s
        player' <- p
        boss' <- b
    | None -> ()

    // boss turn
    for e in effects do
        player' <- applyEffectToPlayer player' e
        boss' <- applyEffectToBoss boss' e

    effects' <- wearOffEffects effects'

    if boss'.HitPoints > 0 then
        let damage = max (boss.Damage - player'.Armor) 1

        player' <-
            { player' with
                  HitPoints = player'.HitPoints - damage }

    (player', boss', effects')

let buy (player: Player) (toBuy: SpellEffect) (effects: Effect list) =
    let (cost, effects', spell) =
        match toBuy with
        | Spell s -> (s.Cost, effects, Some s)
        | Effect e -> (e.Cost, e :: effects, None)

    ({ player with
           Spend = player.Spend + cost
           Mana = player.Mana - cost },
     effects',
     spell)

let playGame (p: Player) (b: Boss) (s: SpellEffect list) =
    let rec game (player: Player) (boss: Boss) (strategy: SpellEffect list) (effects: Effect list) =
        if strategy = List.empty then
            boss.Name
        else
            let toBuy, strategy' = strategy.[0], List.tail strategy
            let (player', effects', spell) = buy player toBuy effects

            if (player'.Mana > 0) then
                let (player'', boss', effects'') = playRound player' boss effects' spell

                if player''.HitPoints <= 0 then
                    boss'.Name
                else if boss'.HitPoints <= 0 then
                    player''.Name
                else
                    game player'' boss' strategy' effects''
            else
                boss.Name

    game p b s List.empty

let getPossibleFollowUpPurchases (purchases: SpellEffect list) =
    seq {
        for s in spells do
            yield (List.append purchases [ Spell s ])

        for e in effects do
            let lifetimeRounds =
                ((float (snd e.Timer)) / 2.) |> ceil |> int

            let eActive =
                if purchases.Length >= (lifetimeRounds - 1) then
                    List.rev purchases
                    |> List.take (lifetimeRounds - 1)
                    |> List.contains (Effect e)
                else
                    false

            if (not eActive) then
                yield (List.append purchases [ Effect e ])
    }
    |> Seq.toList

let calcStrategyCost (strategy: SpellEffect list) =
    strategy
    |> List.sumBy
        (fun p ->
            match p with
            | Spell s -> s.Cost
            | Effect e -> e.Cost)


let bfs player boss =
    let queue = Queue<SpellEffect list>()

    getPossibleFollowUpPurchases List.empty
    |> List.iter queue.Enqueue

    seq {
        while (queue.Count <> 0) do
            let strategy = queue.Dequeue()
            let winner = playGame player boss strategy

            if winner = player.Name then
                yield strategy
            else
                getPossibleFollowUpPurchases strategy
                |> List.iter queue.Enqueue
    }
    |> Seq.head

let createPlayer =
    { Name = "Player"
      HitPoints = 50
      Damage = 0
      Armor = 0
      Mana = 500
      Spend = 0 }


let day22 () =
    let boss =
        InputFile |> System.IO.File.ReadAllLines |> parse

    let player =
        { Name = "Player"
          HitPoints = 50
          Damage = 0
          Armor = 0
          Mana = 500
          Spend = 0 }

    bfs player boss |> calcStrategyCost

module Day22

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

let spellsShop =
    [ { Name = "Magic Missile"
        Cost = 53
        Damage = 4
        Heal = 0 }
      { Name = "Drain"
        Cost = 73
        Damage = 2
        Heal = 2 } ]

let effectsShop =
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

let wearOffEffects (effects: Effect list) (player: Player) =
    let effects' =
        effects
        |> List.map
            (fun e ->
                { e with
                      Timer = ((fst e.Timer) - 1, snd e.Timer) })
        |> List.filter (fun e -> fst e.Timer > 0)

    let shieldInOld =
        effects
        |> List.tryFind (fun e -> e.Name = "Shield")
        |> Option.isSome

    let shieldInNew =
        effects'
        |> List.tryFind (fun e -> e.Name = "Shield")
        |> Option.isSome

    let player' =
        if shieldInOld && not shieldInNew then
            { player with Armor = player.Armor - 7 }
        else
            player

    effects', player'

let buy (player: Player) (toBuy: SpellEffect) =
    let cost =
        match toBuy with
        | Spell s -> s.Cost
        | Effect e -> e.Cost

    { player with
          Spend = player.Spend + cost
          Mana = player.Mana - cost }

let getPossibleFollowUpPurchases manaLeft (activeEffects: Effect list) =
    seq {
        let affordableSpells =
            spellsShop
            |> List.filter (fun s -> s.Cost <= manaLeft)

        for s in affordableSpells do
            yield Spell s

        let affordableEffects =
            effectsShop
            |> List.filter (fun e -> e.Cost <= manaLeft)

        for e in affordableEffects do
            let eActive =
                activeEffects
                |> List.tryFind (fun x -> x.Name = e.Name)
                |> Option.isSome

            if not eActive then yield Effect e
    }
    |> Seq.toList

let createPlayer =
    { Name = "Player"
      HitPoints = 50
      Damage = 0
      Armor = 0
      Mana = 500
      Spend = 0 }

type Difficulty =
    | Easy
    | Hard

let rec play c difficulty (p: Player) (b: Boss) (efx: Effect list) =
    let mutable currentMin = c

    let mutable player =
        match difficulty with
        | Easy -> p
        | Hard -> { p with HitPoints = p.HitPoints - 1 }

    let mutable boss = b
    let mutable effects = efx

    // player turn
    if (player.HitPoints > 0) then
        for e in effects do
            player <- applyEffectToPlayer player e
            boss <- applyEffectToBoss boss e

        let purchases =
            getPossibleFollowUpPurchases player.Mana effects

        let wearedEffects, wearedPlayer = wearOffEffects effects player
        effects <- wearedEffects
        player <- wearedPlayer

        if boss.HitPoints <= 0 then
            if player.Spend < currentMin then
                currentMin <- player.Spend
        else
            for purchase in purchases do
                let mutable player' = player
                let mutable boss' = boss
                let mutable effects' = effects
                player' <- buy player' purchase

                match purchase with
                | Spell boughtSpell ->
                    let pl, bo = applySpell player' boss' boughtSpell
                    player' <- pl
                    boss' <- bo
                | Effect boughtEffect ->
                    player' <- applyEffectToPlayer player' boughtEffect
                    boss' <- applyEffectToBoss boss' boughtEffect

                    let e' =
                        { boughtEffect with
                              Timer = ((fst boughtEffect.Timer) - 1, snd boughtEffect.Timer) }

                    effects' <- List.append effects' [ e' ]

                // boss turn
                if boss'.HitPoints > 0 then
                    for e in effects' do
                        player' <- applyEffectToPlayer player' e
                        boss' <- applyEffectToBoss boss' e

                    if (boss'.HitPoints > 0) then
                        let damage = max (boss'.Damage - player'.Armor) 1

                        player' <-
                            { player' with
                                  HitPoints = player'.HitPoints - damage }

                        if player'.HitPoints > 0
                           && player'.Spend < currentMin then
                            let wearedEffects, wearedPlayer = wearOffEffects effects' player'
                            effects' <- wearedEffects
                            player' <- wearedPlayer

                            let spend =
                                play currentMin difficulty player' boss' effects'

                            if spend < currentMin then
                                currentMin <- spend
                    else if player'.Spend < currentMin then
                        currentMin <- player'.Spend
                else if player'.Spend < currentMin then
                    currentMin <- player'.Spend

    currentMin

let day22 () =
    let boss =
        InputFile |> System.IO.File.ReadAllLines |> parse

    let player = createPlayer
    play System.Int32.MaxValue Easy player boss List.empty

let rec playPart2 difficulty (p: Player) (b: Boss) (efx: Effect list) =
    let mutable player =
        match difficulty with
        | Easy -> p
        | Hard -> { p with HitPoints = p.HitPoints - 1 }

    let mutable boss = b
    let mutable effects = efx

    seq {
        // player turn
        if (player.HitPoints > 0) then
            for e in effects do
                player <- applyEffectToPlayer player e
                boss <- applyEffectToBoss boss e

            let purchases =
                getPossibleFollowUpPurchases player.Mana effects

            let wearedEffects, wearedPlayer = wearOffEffects effects player
            effects <- wearedEffects
            player <- wearedPlayer

            if boss.HitPoints <= 0 then
                yield player.Spend
            else
                for purchase in purchases do
                    let mutable player' = player
                    let mutable boss' = boss
                    let mutable effects' = effects
                    player' <- buy player' purchase

                    match purchase with
                    | Spell boughtSpell ->
                        let pl, bo = applySpell player' boss' boughtSpell
                        player' <- pl
                        boss' <- bo
                    | Effect boughtEffect ->
                        player' <- applyEffectToPlayer player' boughtEffect
                        boss' <- applyEffectToBoss boss' boughtEffect

                        let e' =
                            { boughtEffect with
                                  Timer = ((fst boughtEffect.Timer) - 1, snd boughtEffect.Timer) }

                        effects' <- List.append effects' [ e' ]

                    // boss turn
                    if boss'.HitPoints > 0 then
                        for e in effects' do
                            player' <- applyEffectToPlayer player' e
                            boss' <- applyEffectToBoss boss' e

                        if (boss'.HitPoints > 0) then
                            let damage = max (boss'.Damage - player'.Armor) 1

                            player' <-
                                { player' with
                                      HitPoints = player'.HitPoints - damage }

                            if player'.HitPoints > 0 then
                                let wearedEffects, wearedPlayer = wearOffEffects effects' player'
                                effects' <- wearedEffects
                                player' <- wearedPlayer

                                let spends =
                                    playPart2 difficulty player' boss' effects'

                                for spend in spends do
                                    yield spend
                        else
                            yield player'.Spend
                    else
                        yield player'.Spend
    }

let day22Part2 () =
    let boss =
        InputFile |> System.IO.File.ReadAllLines |> parse

    let player = createPlayer

    playPart2 Hard player boss List.empty
    |> Seq.take 10_000_000
    |> Seq.min

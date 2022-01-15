module GameDvaCharacters

open Props
open DSL
open Dialog
open NPC

type Myself() =
    inherit Person("myself")
    let name = "myself"

    member val Naked = BoolProperty.Personal name "naked" true
    member val HighAsFuck = BoolProperty.Personal name "high" false
    member val IphoneTaken = BoolProperty.Personal name "iphoneTaken" false
    member val IsSporty = BoolProperty.Personal name "sporty" false
    member x.WearSport = x.IsSporty.Set true >> x.Naked.Set false

    member x.WearOffice =
        x.IsSporty.Set false >> x.Naked.Set false

    member x.GetNaked =
        x.IsSporty.Set false >> x.Naked.Set true


type PolicemanJoe() =
    inherit Person("joe")
    let n = "joe"
    member val Talker = Talker(n, DefaultBasicAnswers, DontKnow)

type World(facts: GameDvaFacts.GameDvaFacts) =
    let name = "world"

    member val DeadBodyFoundThisLife = BoolProperty.Personal name "deadFountThisLife" false

    member x.CanStartActionInTheFlat s =
        (facts.shkafSeen.IsKnown s
         && x.DeadBodyFoundThisLife.Get s
         && facts.version.IsKnown s)

    member val PoliceArriving = BoolProperty.Personal name "policeArriving" false
    member val PoliceComingCounter = IterationCounter.Personal name "policeComingCounter"


    member x.PoliceArrive s =
        x.PoliceArriving.Set true s
        |> x.PoliceComingCounter.Start

    member x.PoliceCameAlready s =
        x.PoliceArriving.Get s
        && x.PoliceComingCounter.Elapsed s > 4

type Characters(facts: GameDvaFacts.GameDvaFacts) =
    let myself = Myself()
    member val Myself = myself
    member val PolicemanJoe = PolicemanJoe()
    member val World = World(facts)

    member x.DeathReset(s: State.State) =
        let oldState = s.Data // use this data to save something after death
        { s with State.Data = Map.empty }

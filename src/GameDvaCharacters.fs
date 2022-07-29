module GameDvaCharacters

open Props
open NPC
open Person

type Myself() =
    inherit Person("myself", "Подсознание")
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
    inherit Person("joe", "Полицейський Джо")
    let n = "joe"
    override x.Roles() = 
        RoleModel.RoleModel([
            InLocation(x.Name, "kitchen")
            Talker(x.Name, DefaultBasicAnswers, DontKnow)
        ])
    member val UserKnowsHisName = 
        Facts.createPersonalFact 
            n
            "nameIsKnown"
            "Его зовут Джо"
            "Звание - лейтенант, фамилия - Коперник. Джо Коперник. Как-то так."
    member val knowsAgentCooper = 
        Facts.createPersonalFact
            n
            "knowsCooper"
            "Он знает, что есть некий Агент Купер"
            "И он ничего не знает о личности того самого агента, что можно использовать"
    member val ThinksIAmCooper = 
        BoolProperty.Personal n "thinksIAmCooper" false
    member val ThoughtIWasCooper = 
        BoolProperty.Personal n "thoughtIWasCooper" false
    member val FalledWhenIKickedHim = 
        BoolProperty.Personal n "falledWhenIKickedHim" false
    member val ToldMeSecrets = 
        BoolProperty.Personal n "toldMeSecrets" false

type Babka() =
    inherit Person("babka", "Какая-то старая бабка")
    let n = "babka"
    override x.Roles() = 
        RoleModel.RoleModel([
            InLocation(x.Name, "камера")
            Talker(x.Name, DefaultBasicAnswers, DontCare)
        ])

type World(facts: GameDvaFacts.GameDvaFacts) =
    let name = "world"

    member val DeadBodyFoundThisLife = BoolProperty.Personal name "deadFoundThisLife" false

    member x.CanStartActionInTheFlat s =
        (x.DeadBodyFoundThisLife.Get s
         && facts.version.IsKnown s)

    member val PoliceArriving = BoolProperty.Personal name "policeArriving" false
    member val PoliceComingCounter = IterationCounter.Personal name "policeComingCounter"

    member val talkedToPolice = BoolProperty.Personal name "talkedToPolice" false


    member x.PoliceArrive s =
        x.PoliceArriving.Set true s
        |> x.PoliceComingCounter.Start

    member x.PoliceCameAlready s =
        x.PoliceArriving.Get s
        && x.PoliceComingCounter.Elapsed s > 4

// shortcut
let saved (pers: 'a) = 
    savePers (pers :> Person) |> ignore
    pers

type Characters(facts: GameDvaFacts.GameDvaFacts) =
    let myself = Myself()
    member val Myself = saved myself
    member val PolicemanJoe = saved <| PolicemanJoe()
    member val Babka = saved <| Babka()
    member val Botan = saved <| BasicPerson("botan", "Парень в очках", "камера")
    member val World = World(facts)

    member x.DeathReset(s: State.State) =
        let oldState = s.Data // use this data to save something after death
        { s with State.Data = Map.empty }

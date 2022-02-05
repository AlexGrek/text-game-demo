module GameChapter3

open DSL
open NPC

let init(facts: GameDvaFacts.GameDvaFacts, chars: GameDvaCharacters.Characters) =
    createDialog "police" [
        window "initflat" {
            stxt "На пороге я вижу двоих полицейських. Они как будто не ожидали меня увидеть. Да уж, хуже вряд ли могло быть."
            var (npcDialogVariant "поговорить с ментом" chars.PolicemanJoe.Name)
        }
    ] |> ignore
    
    npc chars.PolicemanJoe {
        stxt "Обычный мусор"
        fact
            facts.afterlife
            DontBelieve
    }
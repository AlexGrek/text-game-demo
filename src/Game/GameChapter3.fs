module GameChapter3

open DSL

let init(facts: GameDvaFacts.GameDvaFacts, chars: GameDvaCharacters.Characters) =
    createDialog "police" [
        window "initflat" {
            stxt "На пороге я вижу двоих полицейських. Они как будто не ожидали меня увидеть. Да уж, хуже вряд ли могло быть."
            // var (pushVariant )
        }

    ]
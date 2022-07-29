module Chapter01

open DSL

let init(facts: VisualNovelFacts.NovelFacts, chars: VisualNovelChars.Chars) = 
    createDialog "init" [
        window "init" {
            stxt """Я проснулся от будильника. Как же хреново. Голова болит, глаза не открываются. На часх 6:01."""
            var ("Попытаться проснуться" --- "игра.начало")
            var ("Зайти в дебаг режим" --- "debug.00")
        }
    ] |> ignore

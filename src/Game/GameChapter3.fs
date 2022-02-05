module GameChapter3

open DSL
open NPC
open State
open Actions

let init(facts: GameDvaFacts.GameDvaFacts, chars: GameDvaCharacters.Characters) =
    let talkedToPolice = Props.BoolProperty("talkedToPolice", false)
    createDialog "police" [
        proxyWindow 
            "initflat"
            (fun (s: State) ->
                if (talkedToPolice.Get s |> not) then
                    doGoToWindow "init1st" 
                else
                    if (chars.Myself.Naked.Get s |> not) then
                        if (chars.PolicemanJoe.ThinksIAmCooper.Get s) then 
                            doJump "arrested.init"
                        else 
                            doJump "agent.init"
                    else
                        doGoToWindow "naked"
                        )
        window "init1st" {
            stxt "На пороге я вижу двоих полицейських. Они как будто не ожидали меня увидеть. Да уж, хуже вряд ли могло быть."
            var (npcDialogActionVariant "поговорить с полицией" chars.PolicemanJoe <| doPushWindow "joe_first.proxy")
            var (hidden talkedToPolice.Get "отлучиться" {
                action doPop
            })
        }
        window "naked" {
            actor chars.PolicemanJoe.Name
            stxt "Наденьте хоть что-то, пожалуйста. На улице не май месяц. И побыстрее."
            var (npcDialogVariant "поговорить с Джо" chars.PolicemanJoe)
            var (hidden talkedToPolice.Get "пойти одеваться" {
                action doPop
            })
        }
    ] |> ignore
    
    npc chars.PolicemanJoe {
        stxt "Полицейський Джо"
        fact
            facts.afterlife
            DontBelieve
        name (fun s ->
            if (chars.PolicemanJoe.UserKnowsHisName.IsKnown s) then
                printfn "FUCK! I know dat fact" 
                "Полицейський Джо"
            else 
                printfn "HOOOOOLY FUCK! I don't know dat fact" 
                "Полицейський" )
    } |> ignore

    createDialog "joe_first" [
        proxyWindow 
            "proxy"
            (fun (s: State) -> 
                if (talkedToPolice.Get s |> not) then
                    doGoToWindow "init"
                else 
                    if (chars.PolicemanJoe.ThinksIAmCooper.Get s) then 
                        doPushWindow "arrested.init"
                    else 
                        doPushWindow "agent.init")
        windowWithActor "init" chars.PolicemanJoe.Name {
            ctxt 
                chars.Myself.Naked.Get
                "Полиция. Лейтенант Джо Коперник. Представьтесь, пожалуйста. Ваши документы."
                "Лейтенант Джо Коперник. Оу! Простите... оденьтесь, пожалуйста, мы из полиции..."
            onEntry chars.PolicemanJoe.UserKnowsHisName.Acquire
            var ("я просто уборщица" -- "неверю")
            var (hidden chars.Myself.Naked.Get "я пойду оденусь" {
                action (doMoveWithStack "init.коридор" [])
            })
            var ("я работаю под прикрытием" -- "следователь")
            var ("я не знаю, как тут оказалась" -- "неверю")
            var ("помогите" -- "помогите")
            var ("знай свое место, щенок" -- "щенок")
        }
        windowWithActor "неверю" chars.PolicemanJoe.Name {
            stxt "Мы это выясним. Вам придется пройти с нами в участок. Собирайтесь."
            var ("не надо в участок" -- "помогите")
            var ("ты че, петушара, попутал?" -- "щенок")
            var ("да это же я, не узнал?" -- "следователь")
            var ("собираюсь" -- "берименя")
        }
        windowWithActor "следователь" chars.PolicemanJoe.Name {
            stxt "Так это вы... Агент Купер... Я не знал, что вы... ну... девушка..."
            onEntry
                (chars.PolicemanJoe.ThinksIAmCooper.Set true
                >> chars.PolicemanJoe.knowsAgentCooper.Acquire)
            var (hidden chars.Myself.Naked.Get "я пойду оденусь" {
                action (doGoToWindow "собирайтесь")
            })
            var (hidden chars.Myself.Naked.Get "хватит пялиться на мои сиськи" {
                action (doGoToWindow "sorry")
            })
            var ("(агент кто?) да, вас должны были предупредить" -- "я не знал")
        }
        windowWithActor "помогите" chars.PolicemanJoe.Name {
            stxt "Вам придется проехать с нами в участок для выяснения личности."
            var ("да это же я, не узнал?" -- "следователь")
            var ("мне нужно собраться" -- "берименя")
        }
        windowWithActor "щенок" chars.PolicemanJoe.Name {
            stxt "Что вы себе позволяете? Как вы можете так себя вести со мной?"
            var ("простите" -- "берименя")
            var ("да это же я" -- "следователь")
        }
        windowWithActor "sorry" chars.PolicemanJoe.Name {
            stxt "Простите... Я лучше отвернусь..."
            var (variant "закончить разговор" { action (doGoToWindow "собирайтесь")})
        }
        windowWithActor "я не знал" chars.PolicemanJoe.Name {
            stxt "Да, меня предупреждали. Вас ждут в участке, мы отвезем вас. Как прошла операция?"
            var (hidden chars.Myself.Naked.Get "хватит пялиться на мои сиськи" {
                action (doGoToWindow "собирайтесь")
            })
            var ("это засекречено" -- "собирайтесь")
            var ("не твоего ума дело" -- "собирайтесь")
            var ("какая еще операция?" -- "собирайтесь")
        }
        windowWithActor "собирайтесь" chars.PolicemanJoe.Name {
            stxt "Ну ладно, не буду вам мешать. Мы подождем тут."
            onEntry (talkedToPolice.Set true)
            var (variant "закончить разговор" { action (doMoveWithStack "init.коридор" [])})
        }
        windowWithActor "берименя" chars.PolicemanJoe.Name {
            stxt "Собирайтесь быстрее."
            onEntry (talkedToPolice.Set true)
            var (variant "закончить разговор" { action (doMoveWithStack "init.коридор" [])})
        }
        
    ] |> ignore
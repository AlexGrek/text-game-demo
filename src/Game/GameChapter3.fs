module GameChapter3

open DSL
open NPC
open State
open Actions

let init(facts: GameDvaFacts.GameDvaFacts, chars: GameDvaCharacters.Characters) =
    let talkedToPolice = chars.World.talkedToPolice
    createDialog "police" [
        proxyWindow 
            "initflat"
            (fun (s: State) ->
                if (talkedToPolice.Get s |> not) then
                    doGoToWindow "init1st" 
                else
                    if (chars.Myself.Naked.Get s |> not) then
                        if (chars.PolicemanJoe.ThinksIAmCooper.Get s) then 
                            doJump "agent.init"
                        else
                            doJump "arrested.init"
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
                "Полицейський Джо"
            else 
                "Полицейський" )
        variants (
            fun s -> 
                if (chars.PolicemanJoe.ThinksIAmCooper.Get s) then
                    [
                        "доложите обстановку, лейтенант" --- "joe_chat_agent.обстановка"
                        "что вообще тут происходит?" --- "joe_chat_agent.втф"
                        "вообще-то я не та, за кого себя выдаю" --- "joe_chat_agent.самозванка"
                        "куда мы едем?" --- "joe_chat_arrested.куда"
                    ]
                else
                    [
                        "что со мной будет?" --- "joe_chat_arrested.чтобудет"
                        "кто вы?" --- "joe_chat_arrested.ктовы"
                        hidden (fun s -> chars.PolicemanJoe.knowsAgentCooper.IsKnown s && (not <| chars.PolicemanJoe.ThoughtIWasCooper.Get s)) "я - агент Купер" {
                            action (doPushWindow "joe_chat_arrested.купер")
                        }
                    ]
        )
    } |> ignore

    createDialog "joe_chat_arrested" [
        windowWithActor "купер" chars.PolicemanJoe.Name {
            stxt """Простите, я вас не узнал. Я не знал, что вы... ну.. женщина..."""
            var (popVariant "эх")
            var (popVariant "ну как так")
            onEntry (chars.PolicemanJoe.ThinksIAmCooper.Set true)
        }
        windowWithActor "чтобудет" chars.PolicemanJoe.Name {
            stxt """Наша задача - доставить вас в участок, а там... суд решит..."""
            var (popVariant "(суд??) ...")
        }
        windowWithActor "ктовы" chars.PolicemanJoe.Name {
            stxt """Я же представился. И значок показывал. У вас что, провалы в памяти?"""
            var (popVariant "(врет, значок он не показывал) нет, ничего")
        }
        windowWithActor "куда" chars.PolicemanJoe.Name {
            stxt """В участок номер семь. А что?"""
            var (popVariant "да нет, ничего")
        }
    ] |> ignore

    createDialog "joe_chat_agent" [
        windowWithActor "обстановка" chars.PolicemanJoe.Name {
            stxt """Эээ... нуу... мы работаем... а что конкретно вас интересует?"""
            var (popVariant "ясно")
            var (popVariant "так держать")
            var ("вы не замечали каких-то странностей?" -- "хз")
            var (hidden (facts.afterlife.IsKnown) "а пространственно-временной континуум в порядке?" {
                action (doGoToWindow "континуум")
            })
        }
        windowWithActor "континуум" chars.PolicemanJoe.Name {
            stxt """Так значит вы что-то знаете... впрочем, не важно. Забудьте. Я простой полицейський, я выполняю приказы."""
            var ("(он что-то скрывает) какие приказы?" -- "приказы")
            var (popVariant "сменить тему")
        }
        windowWithActor "приказы" chars.PolicemanJoe.Name {
            stxt """Ну знаете... Патрулировать, проверить, там, на вызов... еду я, да... в общем... Что мы все обо мне, я 
            то и под прикрытием то никогда не работал."""
            var ("(он определенно что-то скрывает) отвечай" -- "приказы2")
            var (popVariant "сменить тему")
        }
        windowWithActor "приказы2" chars.PolicemanJoe.Name {
            stxt """Простите, мне нечего вам сказать."""
            var (popVariant "ладно, я так ничего с него не добъюсь")
        }
        windowWithActor "хз" chars.PolicemanJoe.Name {
            stxt """Да вроде все как обычно, только начальство какое-то нервное в последнее время. 
            Как будто их всех премии лишили, хе-хе."""
            var (popVariant "(это, типа, шутка?) улыбнуться")
            var (popVariant "сменить тему")
        }
        windowWithActor "втф" chars.PolicemanJoe.Name {
            stxt """Я приехал по поручению Капитана. Мы знали, что наш агент тоже может быть на месте.
            Надеюсь, вы успели все сделать вовремя. Не знаю вашей задачи, но мне и не положено. Можете на меня рассчитывать, если 
            понадобится помощь. Я неплохо играю в боулинг, хе-хе. Ну, хорош, типа, в этом. Как бы."""
            var (popVariant "(что-то с ним не так) улыбнуться")
            var (popVariant "поблагодарить за информацию")
            var (hidden (facts.afterlife.IsKnown) "" {
                text "хватит дурака валять. Что тут нахрен творится?"
                action (doOnce
                            "dopros_joe"
                            (doPopPush "dopros_joe.init")
                            (doGoToWindow "dopros_fail"))
            })
        }
        windowWithActor "dopros_fail" chars.PolicemanJoe.Name {
            stxt """Я и так сказал вам все, что знаю. Я рискую слишком многим. Простите, я больше ничего не скажу."""
            var (popVariant "ладно")
        }
        windowWithActor "самозванка" chars.PolicemanJoe.Name {
            stxt """Я так и думал. Вы арестованы до выяснения личности."""
            onEntry (chars.PolicemanJoe.ThinksIAmCooper.Set false >> chars.PolicemanJoe.ThoughtIWasCooper.Set true)
            var (popVariant "(и нахрена я это сделала)")
            var (popVariant "(за то моя совесть чиста)")
        }
    ] |> ignore

    createDialog "dopros_joe" [
        windowWithActor "init" chars.PolicemanJoe.Name {
            stxt """Ладно, ладно, я скажу, что знаю. Что вы хотите узнать?"""
            onEntry (chars.PolicemanJoe.ToldMeSecrets.Set true)
            var (popVariant "мне больше ничего не интересно")
            var ("кто тобой командует?" -- "капитан")
            var ("теряют ли люди память?" -- "память")
            var ("дежавю" -- "дежавю")
            var ("напарник" -- "напарник")
            var ("что это за место" -- "место")
        }
        windowWithActor "капитан" chars.PolicemanJoe.Name {
            stxt """Капитан! Капитан полиции! Он настоящий капитан, честно! Седьмой участок, номер значка... четыре... два..."""
            var ("ясно, хватит" -- "init")
        }
        windowWithActor "память" chars.PolicemanJoe.Name {
            stxt """У меня бывают провалы в памяти иногда, честно говоря. Это останется между нами, ладно?"""
            var ("да, я обещаю, подробнее пожалуйста" -- "память2")
            var ("ясно, хватит" -- "init")
        }
        windowWithActor "память2" chars.PolicemanJoe.Name {
            stxt """Иногда я просыпаюсь и не могу вспомнить, кто я... но это быстро проходит. У меня... по крайней мере... Говорят, 
            у некоторых не проходит, но со мной все хорошо, честно! Я клянусь, я все осознаю. Все хорошо. Со мной все хорошо."""
            var ("(похоже, с ним все не очень хорошо) ок" -- "init")
            var ("советую сходить к психиатру" -- "init")
        }
        windowWithActor "дежавю" chars.PolicemanJoe.Name {
            stxt """Не понимаю, о чем вы. Со мной такого не бывало. Ходят слухи... про... ну... восстание... из мертвых...
            но это полная херня, мне кажется. Придурков хватает, хе-хе."""
            var ("поняла" -- "init")
        }
        windowWithActor "напарник" chars.PolicemanJoe.Name {
            stxt """Это Джек. Его у нас называют Молчаливый Джек, хе-хе. Он редко говорит. После одного события... Ну, это личное, понимаете. 
            Теперь он почти не говорит. И не удивляется уже ничему. Такая у нас работа, у простых копов. Что ж там у вас - страшно представить."""
            var ("спасибо" -- "init")
        }
        windowWithActor "место" chars.PolicemanJoe.Name {
            stxt """Место? Хм. Это дом по улице Джареда Лето. Такой же, как другие, вроде. Этажей дохрена, хе-хе, за то хаты, говорят, недорогие.
            Но это не точно, особенно глядя на эту, хе-хе."""
            var ("(странный тип) понятно" -- "init")
        }
    ] |> ignore

    createDialog "joe_first" [
        proxyWindow 
            "proxy"
            (fun (s: State) -> 
                if (talkedToPolice.Get s |> not) then
                    (doGoToWindow "init").ComposeAfter(facts.policeIsComing.Acquire)
                else 
                    if (chars.PolicemanJoe.ThinksIAmCooper.Get s) then 
                        doPushWindow "agent.init"
                    else 
                        doPushWindow "arrested.init")
        windowWithActor "init" chars.PolicemanJoe.Name {
            ctxt 
                (chars.Myself.Naked.Get >> not)
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

    createDialog "agent" [
        window "init" {
            stxt """Мне придется поехать в участок, других вариантов мне, похоже, не оставили. 
            По крайней мере я еду туда не как узница, а как уважаемый человек. Агент. Но кто же я на самом деле?"""
            var ("сесть в полицейськую машину" -- "машина")
            var ("сказать, что доберусь своим ходом" -- "своим")
        }
        window "своим" {
            actor chars.PolicemanJoe.Name
            stxt """Простите, но это исключено. Нам приказоно доставить вас в участок незамедлительно. Прошу, садитесь в машину."""
            var ("сесть в полицейськую машину" -- "машина")
            var ("послать его нахер" -- "нахер")
        }
        window "нахер" {
            stxt """Простите, но мы не можем не привезти вас. Это приказ сверху. Мы не можем не подчиниться.
            И это приказ вам тоже. Обсудите это с руководством на месте. Поехали."""
            var ("сесть таки в полицейськую машину" -- "машина")
        }
        window "машина" {
            stxt """Полицейськая машина кажется такой знакомой изнутри."""
            var (npcDialogVariant "поговорить с Джо" chars.PolicemanJoe)
            var (variant "доехать до участка" { action (doMoveWithStack "участок.proxy" [])})
        }
    ] |> ignore

    createDialog "arrested" [
        window "init" {
            stxt """Меня ведут в полицейськую машину.
            В то же время команда криминалистов входит в дом. Как мне выбираться из этой ситуации?"""
            var ("попытаться сбежать" -- "вырвалась")
            var ("послушно идти вперед" -- "машина")
        }
        window "вырвалась" {
            stxt "Похоже, сейчас отличный момент, чтобы..."
            var ("толкнуть Джо и вырваться" -- "рвануть")
            var ("продолжить послушно идти вперед" -- "машина")
        }
        window "рвануть" {
            stxt "Мне удалось свалить крупного мужика на землю без особых проблем."
            onEntry (chars.PolicemanJoe.FalledWhenIKickedHim.Set true)
            var (variant "бежать" {
                action
                    (doCond (chars.Myself.IsSporty.Get) (doGoToWindow "побег") (doGoToWindow "падение"))
            })
            var ("извиниться" -- "извините")
        }
        window "извините" {
            actor chars.PolicemanJoe.Name
            stxt "Мы с тобой еще не закончили. Добром это не кончится."
            var ("послушно идти вперед" -- "машина")
        }
        window "побег" {
            stxt "Я пускаюсь со всех ног бежать, не оборачиваясь."
            var ("бежать" -- "побег2")
        }
        window "побег2" {
            actor chars.PolicemanJoe.Name
            stxt "А ну стоять, сука!"
            var ("не оглядываться" -- "побег3")
        }
        window "побег3" {
            actor chars.PolicemanJoe.Name
            stxt "Стрелять буду!!!"
            var ("не оглядываться" -- "побегвсе")
            var ("сдаться" -- "извините")
        }
        window "побегвсе" {
            stxt """Выстрел... похоже, это был предупредительный. Еще выстрел..."""
            var (variant "" {
                text "..."
                action (doPushWindow "смерть.init")
                modify (facts.policeKillsMe.Acquire)
            })
        }
        window "падение" {
            stxt """Я наступила на собственную штанину и позорно шлепнулась.
            Надо было не терять свою одежду, или хотя бы выбрать более адекватное что-то, спортивный костюм, например.
            Я сейчас начну плакать от того, насколько это обидно."""
            var ("смириться с поражением" -- "извините")
        }
        window "машина" {
            stxt """Меня посадили в классический полицейський "Суцидо Такома". Почему-то этот интерьер показался мне знакомым. 
            Возможно, я не первый раз попадаю в неприятности."""
            var ("ну поехали" -- "ехать")
        }
        window "ехать" {
            stxt """Джо сидит рядом и не спускает с меня глаз. Наверняка, дело тут не в моей внешности."""
            var ("продолжить ехать молча" -- "ехать2")
            var (npcDialogVariant "поговорить с Джо" chars.PolicemanJoe)
        }
        window "ехать2" {
            stxt """Мы добрались до участка. Интересно, какая судьба меня ждет?"""
            var (variant "послушно пройти в участок" { action (doMoveWithStack "участок.proxy" [])})
        }
    ] |> ignore

    createDialog "участок" [
        proxyWindow 
            "proxy"
            (fun (s: State) ->
                    if (chars.PolicemanJoe.ThinksIAmCooper.Get s |> not) then
                        doPushWindow "участок.вкамеру"
                    else
                        doPushWindow "участок.центр")
        window "вкамеру" {
            stxt "Меня отвели в камеру к каким-то бомжам. Вонь, задуха... жопа, одним словом. И ничего не сказали."
            var (pushLocVariant "осмотреться" "камера")
        }
        window "центр" {
            stxt """Я сижу в центре полицейського участка. Жду аудиенции у капитана.
            Вокруг бегают копы с какими-то бумажками, за дальним столиком следак сидит и всматривается в монитор, попивая кофе.
            На меня даже внимания никто не обращает. Хотя я довольно странно одета, как для этого места. Да и вообще странно."""
            var (pushLocVariant "осмотреться" "участок")
        }
    ] |> ignore

    location "камера" {
        stxt """Ну и вонь. А люди вокруг, если присмотреться, на бомжей не похожи. Может, стоит наладить с ними контакт."""
    } |> ignore

    location "участок" {
        stxt """Радует, что я не в камере, но полицейський участок - тоже не лучшее место, где можно оказаться после того, как
        тебя нашли на месте убийства. К тому же, я не помню ничего, как я буду защищаться, если что?"""
    } |> ignore

    npc chars.Babka {
        allow PersonHub.AllowedInteractions.Nothing
        stxt "Бабка не выглядит дружелюбной. Она похожа на старую циганку, которая может оставить тебя без денег на вокзале."
        var ("подойти" --- "бабка.идиты")
    } |> ignore

    createDialog "бабка" [
        window "идиты" {
            actor chars.Babka.Name
            stxt "Иди ты отсюдова"
            var (popVariant "отойти")
        }
    ] |> ignore

    npc chars.Botan {
        allow PersonHub.AllowedInteractions.OnlyTalk
        startonce "botan.initial"
        stxt "Бабка не выглядит дружелюбной. Она похожа на старую циганку, которая может оставить тебя без денег на вокзале."
        var ("подойти" --- "бабка.идиты")
    } |> ignore

    createDialog "botan" [
        window "initial" {
            // actor chars.Botan.Name
            stxt "Молодой парень в очках сидит и бубнит что-то себе под нос, не обращая никакого внимания на всех вокруг. Создает впечатление психа."
            var (popVariant "отойти")
            var ("попробовать поговорить" -- "поговорить")
        }
        window "поговорить" {
            actor chars.Botan.Name
            stxt "Слишком много вопросов. Слишком много вопросов. Слишком много вопросов. Слишком много..."
            var (popVariant "отойти")
            var (popVariant "точно отойти, ну его")
        }
    ] |> ignore

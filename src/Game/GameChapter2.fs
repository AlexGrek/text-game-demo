module GameChapter2

open DSL

let init(facts: GameDvaFacts.GameDvaFacts, chars: GameDvaCharacters.Characters) =
    let iPhoneUnlock = Props.BoolProperty("iPhoneUnlockAttempt", false)
    let hasPhoneInPicketOrInInventory = (fun s -> facts.phoneFprints.IsKnown s || chars.Myself.IphoneTaken.Get s) 
    createDialog "осмотр" [
        window "личность" {
            stxt """Мужик мужиком. Возраст, думаю, около 30.
            Ухоженный, спортивный, с легкой щетиной. Вот только немного мертвый. 
            Его небесно-голубые глаза все еще открыты, безжизненно смотрят в потолок.
            Лицо сковала гримаса ужаса. Скорее всего, у него были 
            другие планы на этот вечер. Он определенно не планировал умирать.
            Его имя, род занятий и прочее я не могу даже предположить. 
            Вроде не бомж. (К сожалению. Его точно будут искать. А значит, вероятно, и 
            меня тоже...)
            """
            var (variant "посмотрим еще" {
                action doPop
            })
            var (variant "какая нелепая смерть" {
                action doPop
            })
            var (variant "может в карманах что завалялось" {
                action (doGoToWindow "карманы")
            })
        }
        window "карманы" {
            stxt """
            Копаться в карманах мертвеца? Серьезно?
            """
            var (popVariant "нет, это бред")
            var ("да" -- "карманы2")
        }
        window "карманы2" {
            stxt """
            В левом кармане штанов... пусто, что же в правом...
            """
            var (popVariant "нет, я не буду, это бред")
            var (variant "что же?" {
                    action (doOnce "iphonefound" (doGoToWindow "карманы3") (doGoToWindow "айфон найден уже"))
                })
        }
        window "карманы3" {
            stxt """
            Телефон! Это успех! Айфон последней модели. Стоп, я же на нем оставила свои отпечатки! Вот я дура. 
            Ладно, одних отпечатков ног в этой квартире уже хватило бы, чтобы меня посадить. Надеюсь, на ноже 
            моих отпечатков не будет.
            """
            var (variant "оставить его в кармане, как было" {
                action doPop
                modify (facts.phoneFprints.Acquire)
                modify (chars.Myself.IphoneTaken.Set false)
            })
            var (variant "взять и попытаться его разблокировать" {
                action (doGoToWindow "айфон")
                modify (facts.phoneFprints.Deny)
                modify (chars.Myself.IphoneTaken.Set true)
            })
        }
        window "айфон найден уже" {
            ctxt hasPhoneInPicketOrInInventory
                "Тут я находила телефон. Что мне с ним делать сейчас?"
                "Телефона больше нет."
            var (locked "потерян телефон" hasPhoneInPicketOrInInventory "оставить его в кармане, как было" {
                action doPop
                modify (facts.phoneFprints.Acquire)
                modify (chars.Myself.IphoneTaken.Set false)
            })
            var (locked "потерян телефон" hasPhoneInPicketOrInInventory "взять его" {
                action 
                    (doCond (iPhoneUnlock.Get >> not)
                        (doGoToWindow "айфон")
                        (doGoToWindow "айфон попытка"))
                modify (facts.phoneFprints.Deny)
                modify (chars.Myself.IphoneTaken.Set true)
            })
            var (hidden (hasPhoneInPicketOrInInventory >> not) "" {
                text "уйти"
                action doPop
            })
        }
        window "айфон" {
            stxt """
            Он требует пин код. Лицом уже не разблокируешь. Интересно, а мертвеца он распознал бы, или нет?
            Уже не узнаем. Пин кода я не знаю. Может, 3428? Гадать бесполезно. Остается только взять его с собой.
            """
            var (variant "оставить его в кармане, как было" {
                action doPop
                modify (facts.phoneFprints.Acquire)
                modify (chars.Myself.IphoneTaken.Set false)
            })
            var (locked "уже попытались" (iPhoneUnlock.Get >> not) "набрать 3428" {
                action (doGoToWindow "айфон попытка")
                modify (iPhoneUnlock.Set true) 
            })
            var (locked "уже попытались" (iPhoneUnlock.Get >> not) "набрать 0451" {
                action (doGoToWindow "айфон попытка")
                modify (iPhoneUnlock.Set true) 
            })
            var (locked "уже попытались" (iPhoneUnlock.Get >> not) "набрать 1111" {
                action (doGoToWindow "айфон попытка")
                modify (iPhoneUnlock.Set true) 
            })
            var (locked "уже попытались" (iPhoneUnlock.Get >> not) "набрать 1234" {
                action (doGoToWindow "айфон попытка")
                modify (iPhoneUnlock.Set true)
            })
            var (locked "уже попытались" (iPhoneUnlock.Get >> not) "набрать 1337" {
                action (doGoToWindow "айфон попытка")
                modify (iPhoneUnlock.Set true)
            })
            var (popVariant "взять с собой")
        }
        window "айфон попытка" {
            stxt """
            Осталось две попытки! Все, я больше не буду пытаться угадывать, это безнадежно. Может, еще узнаю код. 
            Пропущенных на экране вроде нет, слава богу. Значит, у меня еще есть время. 
            """
            var (popVariant "взять с собой")
            var (variant "положить его в карман, как было" {
                action doPop
                modify (facts.phoneFprints.Acquire)
                modify (chars.Myself.IphoneTaken.Set false)
            })
        }
        window "орудие" {
            stxt """
            Обычный кухонный нож. Для мяса. Нужен хорошо наточенный нож, чтобы он мог войти в грудь настолько глубоко.
            Пробита мышца, пробито ребро. Удар был очень сильным, сомневаюсь, что я смогла бы его нанести.
            Думаю, как детектив. Может, я и есть... Бред. 
            Следов борьбы нет. Он не ожидал удара. Что ж, на самозащиту это тоже не похоже. 
            Нет никакого беспорядка вокруг, нет следов от ногтей. У меня под ногтями (глянем...) тоже ничего нет.
            Один удар. Смертельный. В сердце. Работа профессионала. Или просто повезло. 
            """
            var (popVariant "интересно")
            var ("так кто же я?" -- "вспомнить")
        }
        window "монолог" {
            stxt """Так, что я знаю о себе... да ничего. Как такое вообще может быть?
            Я не помню, как меня зовут. Я не помню, что я тут делала.
            Это... я его... ?
            """
            var (popVariant "не хочу думать об этом")
            var ("я защищалась" -- "защищалась")
            var ("давай же, вспомни хоть что-то" -- "вспомнить")
        }
        window "защищалась" {
            stxt """Может это вообще была не я? Кто-то пришел и убил его, а еще украл всю мою одежду. 
            Звучит правдоподобно. Разве нет?"""
            var ("нет, не очень правдоподобно" -- "вспомнить")
            var ("да, звучит отлично" -- "вспомнить")
            var ("может все же защищалась" -- "девушка")
            var (popVariant "не хочу думать об этом, мне аж плохо становится")
        }
        window "вот я кто" {
            stxt """Значит, вот я кто. Но это не точно. Ладно, вспомнить я еще успею. Надеюсь. Сейчас займемся другими вещами."""
            var ("предположить другой вариант" -- "вспомнить")
            var (hidden (facts.version.IsKnown) "" {
                text "вернуться к телу"
                action doPop
            })
        }
        window "вспомнить" {
            stxt """Как вообще можно было забыть... Ладно, на это нет времени. Кто же я... Идеи?
            """
            var (hidden (facts.version.IsKnown) "" {
                text "вернуться к телу"
                action doPop
            })
            var ("проститутка" -- "проститутка")
            var ("частный детектив" -- "детектив")
            var ("жена" -- "жена")
            var ("просто приличная девушка" -- "девушка")
        }
        window "проститутка" {
            stxt """Если я проститутка, то как объяснить тело? Что такого могло произойти, что я убила его? Или это была не я? Так, а есть еще идеи?
            """
            var (popVariant "не могу думать об этом, надо решать вопросы")
            var ("точно проститутка" -- "точно проститутка")
            var ("частный детектив" -- "детектив")
            var ("жена" -- "жена")
            var ("просто приличная девушка" -- "девушка")
        }
        window "точно проститутка" {
            stxt """Нет, я бы не забыла такое. Не могла. Не проститутка я. Они иначе выглядят. Наверное.
            Точно не будет других вариантов?
            """
            var ("нет, я определенно проститутка" -- "вот я кто")
            var ("частный детектив" -- "детектив")
            var ("жена" -- "жена")
            var ("просто приличная девушка" -- "девушка")
        }
        window "девушка" {
            onEntry facts.version.Acquire
            stxt """В это я готова поверить. Он подцепил меня в клубе, мы приехали к нему, он начал приставать... 
            Подсыпал мне что-то, потому мне так херово сейчас. Я потеряла контроль и убила его случайно.
            Это объяснило бы, почему я ничего не помню. Но тогда у меня проблемы.
            """
            var (popVariant "не могу думать об этом, надо решать вопросы")
            var ("лучше уж проститутка" -- "проститутка")
            var ("частный детектив" -- "детектив")
            var ("жена" -- "жена")
            var ("да, это похоже на правду" -- "вот я кто")
        }
        window "детектив" {
            stxt """Какой бред, господи. Какой еще детектив? Как мне это в голову пришло? Я тут расследую убийтсво? Голая? 
            Или, может, я по супружеским изменам? Или по неуплате аллиментов...
            """
            var (popVariant "не могу думать об этом, надо решать вопросы")
            var ("та не, вполне может быть" -- "вот я кто")
            var ("проститутка я, не иначе" -- "точно проститутка")
            var ("жена" -- "жена")
            var ("просто приличная девушка" -- "девушка")
        }
        window "жена" {
            stxt """Не похоже это место на уютное семейное гнездышко. Тут нет ни одной моей вещи. Я даже трусов своих
            не нашла, а если я тут живу - то их бы определенно было много. Может, я живу по соседству? Странные отношения.
            А убила я его из ревности? И голова почему так болит... Нет, я тут точно не была раньше. Еще варианты?
            """
            var (popVariant "не могу думать об этом, надо решать вопросы")
            var ("похоже, таки проститутка" -- "проститутка")
            var ("частный детектив" -- "детектив")
            var ("просто приличная девушка" -- "девушка")
        }
        window "одежда" {
            ctxt (facts.shkafSeen.IsKnown) """Нет, снимать одежду с мертвеца я точно не буду.
            Брюки, голубоватая рубашка. Это выглядит дорого. И все залито кровью.
            """ """На нем такая же рубашка и брюки, как те, что я видела в шкафу. 
            Можно сделать вывод, что он тут жил. А вот я определенно живу не здесь."""
            var (popVariant "ясно")
            var (popVariant "может, в шкаф заглянуть")
        }
    ] |> ignore

    createDialog "снаружи" [
        window "init" {
            stxt """Ты оказалась в центре длинного коридора. Десятки, а может и сотни дверей человейника 
            смотрят на тебя с недоверием. Кто ты, зачем нарушаешь их спокойствие? Узнать бы самой."""
            var ("пойти налево" -- "лево")
            var ("пойти направо" -- "право")
            var (popVariant "вернуться в квартиру")
        }
        window "лево" {
            stxt """Ты находишь площадку с лифтами. Один грузовой, один маленький. 
            Как такое количество лифтов умудряется обслуживать такой высокий дом? Тут же куча квартир. 
            Может, с другой стороны тоже есть лифты."""
            var ("вызвать лифт" -- "вызвать")
            var ("снять штаны и повернуться к лифту задом" --- "што.снять штаны")
            var ("уйти обратно в коридор" -- "init")
            var (popVariant "вернуться в квартиру")
        }
        window "право" {
            stxt """Ты находишь лестничную клетку. Табличка красноречиво намекает, что вниз еще 20 этажей.
            Сколько там вверх... да кого это волнует. Мне, скорее всего, вниз.
            Может, с другой стороны есть лифты, и это было бы более разумно."""
            var ("идти верх" --- "лестница.вверх")
            var ("идти вниз" --- "лестница.вниз")
            var ("уйти обратно в коридор" -- "init")
            var (popVariant "вернуться в квартиру")
        }
        window "вызвать" {
            stxt """Лифт едет."""
            var ("просто подождать лифт" -- "лифт приехал")
            var ("снять штаны и повернуться к лифту задом" --- "што.снять штаны")
            var ("забить и уйти обратно в коридор" -- "init")
            var (popVariant "вернуться в квартиру")
        }
        window "лифт приехал" {
            stxt """Двери лифта открылись. К счастью, в лифте никого нет. Что мне сделать?"""
            var ("войти" --- "лифт.init")
            var ("снять таки штаны и повернуться к лифту задом" --- "што.снять штаны")
            var ("идти в коридор" -- "init")
            var (popVariant "вернуться в квартиру")
        }
    ] |> ignore
    createDialog "лифт" [
        window "init" {
            stxt "Лифт позволяет ехать куда угодно, но куда угодно мне не надо."
            var (variant "на первый" {
                    action (doOnce 
                        "strangeLiftManHappened"
                        (doGoToWindow "первый")
                        (doGoToWindow "первый просто"))
                    })
            var ("на последний" -- "последний")
            var (popVariant "выйти из лифта")
        }
        window "последний" {
            stxt """Поездка наверх прошла без приключений. Стоп, это что за звук? Какой-то грохот..."""
            var ("..." -- "последний2")
        }
        window "последний2" {
            stxt """Похоже на звук... выстрела? Так близко... Лифт уже начал останавливаться. 
            Как остановить лифт, где тут кнопка..."""
            var ("меня сковал страх" -- "последний3")
            var ("нажать кнопку \"стоп\"" -- "последний3")
            var (hidden (facts.afterlife.IsKnown) "в этот раз я точно покажу ему" {
                action (doGoToWindow "последний4")
            })
        }
        window "последний3" {
            stxt """Слишком... поздно... Лифт открывается. Я вижу фигуру в капюшоне сквозь едва открывшуюся дверь. 
            У него... ПИСТОЛЕТ, ОН НАПРАВЛЕН НА МЕНЯ"""
            var ("аааааааааааааааа" -- "последний4")
            var ("вспомнить уроки кунг фу" -- "последний4")
        }
        window "последний4" {
            stxt """Оглушительный шум... резкая боль... темнеет в глазах... неужели это конец..."""
            var ("отбросить копыта" --- "смерть.init")
            var ("отдать богу душу" --- "смерть.init")
            onEntry (facts.strangerKillsMe.Acquire)
        }
        window "первый" {
            stxt """На следующем же этаже ко мне в лифт вошел какой-то лысый хрен. Стоит тут. Поехали вниз.
            Он даже не смотрит в мою сторону, но я как будто чувствую, как его мысли впиваются в меня.
            Он изучает меня. Он хочет понять, кто я такая."""
            var ("..." -- "первый2")
        }
        window "первый2" {
            stxt """Огромный, как охранник ночного клуба. Лысый, как бизнесмен из 90х, который ничего
            не понимает в бизнесе, но отлично понимает в разбивании голов. И одет так же.
            За всю поездку он не проронил ни слова. Двери лифта открылись, но меня как будто сковало.
            Мужчина вышел и пошел по своим делам."""
            var ("ехать на верхний этаж" -- "последний")
            var (popVariant "вернуться в квартиру")
            var ("выйти на улицу" --- "улица.init")
        }
        window "первый просто" {
            stxt """Я спокойно в одиночестве добралась до первого этажа. Ничего не произошло. Фух."""
            var ("ехать на верхний этаж" -- "последний")
            var (popVariant "вернуться в квартиру")
            var ("выйти на улицу" --- "улица.init")
        }
    ] |> ignore
    createDialog "смерть" [
        window "init" {
            onEntry (chars.DeathReset)
            rand [ 
                "Твое время еще не пришло..."
                "Все еще впереди..."
                "Используй свой шанс..."
                "Не в этот раз..."
                "Я тебя не отпущу..."
                "Час еще не настал..."
                "Прикоснись к вечности..."
                "Твое призвание в другом..."
                "Учись..."
                "Свет в конце тунеля..."
                ]
            actor "Шепот во тьме"
            var (variant "шагнуть во тьму" {
                action (doCond (facts.afterlife.IsKnown >> not)
                            (doGoToWindow "firstTime")
                            (doGoToWindow "onceAgain"))
            })
        }
        window "firstTime" {
            stxt "Я... мертва? Что происходит? Это сон? Это реальность? А где ангелочк... что? Что происходит?"
            var (variant "проснуться" {
                action (doMoveWithStack "init.init" [])
                modify (facts.afterlife.Acquire)
            })
        }
        window "onceAgain" {
            stxt "Я мертва. Снова. Неужели все начнется заново?"
            var (variant "проснуться" {
                action (doMoveWithStack "init.init" [])
                modify (facts.afterlife.Acquire)
            })
            var (variant "постараться не думать о смерти" {
                action (doMoveWithStack "init.init" [])
                modify (facts.afterlife.Acquire)
            })
        }
    ] |> ignore
    createDialog "што" [
        window "снять штаны" {
            stxt """Ты это серьезно? Господи, какой идиот писал варианты для этой игры."""
            var ("да, снимай давай" -- "сделать дичь")
            var (popVariant "простите")
        }
        window "сделать дичь" {
              stxt "Ну ладно. Я это сделала. Зачем?"

              var (
                  variant "" {
                      text "перестать заниматься ерундой"
                      action doPop
                  }
              )
          }
    ] |> ignore
    createDialog "лестница" [
        window "вверх" {
            stxt """Я начала идти вверх. Иду вверх. Иду. Иду..."""
            var ("иду" -- "вверх1")
            var (popVariant "вернусь ка я лучше, я устала")
        }
        window "вниз" {
            stxt """Иду вниз. Давно я столько не ходила по ступенькам. Даже в зале так не устаю. Насколько я могу вспомнить...
            Каком зале... Как меня зовут то вообще..."""
            var ("иду и не думаю" -- "вниз1")
        }
        window "вниз1" {
            stxt """Да уж. Этот дом как будто не заканчивается."""
            var ("выйти на улицу" --- "улица.init")
            var (popVariant "вернуться на этаж")
        }
        window "вверх1" {
            stxt """Можно будет недельку не приседать после этого. А может, и не недельку."""
            var ("иду" -- "вверх2")
        }
        window "вверх2" {
            stxt """Лестница выглядит довольно однообразной. Обувь на 10 размеров больше моей - такое себе удовольствие
            для передвижения по лестницам. Ноги болят. Нужно сделать перерыв."""
            var ("иду" -- "вверх3")
        }
        window "вверх3" {
            stxt """Сделала перерыв. Не очень помогло. Так, еще рывок... Надо было считать этажи, наверное. 
            Понятия не имею, сколько я уже прошла. Месяц в зал не пойду после такого. А обувь... лучше босиком."""
            var ("иду" -- "вверх4")
        }
        window "вверх4" {
            stxt """Какие-то странные звуки сверху. Как будто выстрел... Резкий такой, звонкий... Что там происходит?"""
            var ("иду" -- "вверх пришла")
        }
        window "вверх пришла" {
            stxt """Тссс... кажется, меня заметили. Кто-то стремительно приближается к лестничной клетке. Что делать?"""
            var ("попробовать выбить оружие из рук" -- "выбить")
            var ("бежать вниз" -- "вверх бежать")
        }
        window "выбить" {
            stxt """Ловким движением руки я выбила пистолет из рук человека в капюшоне. Он явно не ожидал такого
            поворота, так что у меня было преимущество. И откуда я так умею? Пистолет отскочил от стены и выстрелил..."""
            var ("что со мной?" -- "выбить смерть")
        }
        window "выбить смерть" {
            stxt """Шок от громкого звука прошел... кровь наполнила рот, я начинаю задыхаться... Сознание покидает меня, 
            я падаю и..."""
            var (variant "отпустить эту жизнь" {
                action (doPushWindow "смерть.init")
                modify (facts.strangerKillsMe.Acquire)
            })
        }
        window "вверх совсем" {
            stxt "Так, что тут у нас... О нет, меня заметили! У него ствол! Надо бежать!"
            var ("бежать вниз по лестнице" -- "вверх бежать")
        }
        window "вверх бежать" {
            stxt """Быстрее, вниз! Ооо нееет, он открыл дверь, он бежит за мной!"""
            var ("остановиться передохнуть" -- "вверх бежать3")
            var ("бежать быстрее" -- "вверх бежать2")
        }
        window "вверх бежать2" {
            stxt """Оглянулась... чуть не споткнулась! Там какой-то хрен в черном. Он точно хочет меня убить.
            Нужно еще быстрее."""
            var ("бежать быстрее" -- "вверх бежать3")
        }
        window "вверх бежать3" {
            stxt """Звон в ушах... в меня выстрелили! Тот тип уже близко, я успела разглядеть лишь его черный капюшон.
            Пуля отскочила от стены. Косой мазила, надеюсь ты потратишь все патро... Еще выстрел... АААААА!!!"""
            var ("упасть" -- "вверх бежать4")
            var ("упасть, других вариантов не будет" -- "вверх бежать4")
        }
        window "вверх бежать4" {
            stxt """Дикая боль в ноге сковала движения, я просто полетела с лестницы вниз. Еще выстрел...
            Темнеет в глазах... Звон... Боль... Теплота..."""
            var (variant "" {
                text "умереть"
                action (doPushWindow "смерть.init")
                modify (facts.strangerKillsMe.Acquire)
            })
        }
    ] |> ignore
    createDialog "улица" [
        window "init" {

            stxt """На улице спокойно. Только полицейський фургон заливает двор ярким светом своей синей мигалки. 
            Двор похож скорее на огромную автостоянку. Впрочем, как и везде. Автостоянка..."""
            var (popVariant "зайти внутрь, тут пока делать нечего")
        }
    ] |> ignore

    (facts, chars)


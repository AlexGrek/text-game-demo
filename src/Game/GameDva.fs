module GameDva

open State
open DSL
open NPC
open Actions

let INITIAL_STATE =
    makeInitialStateInDialog { D = "init"; W = "init" }

// define all the game logic
let build () =
    Engine.reset ()
    let facts = GameDvaFacts.GameDvaFacts()
    let chars = GameDvaCharacters.Characters(facts)

    let wInit =
        window "init" {
            stxt
                """Ты кое-как просыпаешься. Ты не помнишь, что было вчера. Голова болит.
            Едва протерев глаза, ты замечаешь, что тебя окружает незнакомая квартира."""

            var ("постараться уснуть и доспать" -- "2")
            var ("вскочить и осмотреться" -- "2")
            var ("это всего лишь сон" -- "2")
            var (
                hidden (facts.afterlife.IsKnown) "что со мной только что произошло" {
                    action (doGoToWindow "я жива")
                }
            )
        }

    let w2 =
        window "2" {
            stxt
                """Ты замечаешь, что на тебе совершенно нет одежды. И твоя грудь, довольно внушительных размеров,
        ничем не прикрыта. Да, ты девушка. Да еще какая. Вспомнить бы только, как эту девушку зовут..."""

            var (
                variant "встать" {
                    text "посмотреть вокруг"
                    action (doGoToWindow "коридор")
                }
            )

            var (pushLocVariant "сразу на кухню, на новую нахуй" "kitchen")

            var (
                variant "2" {
                    text "попытаться все же уснуть"
                    action (doGoToWindow "init")
                }
            )
            var (
                hidden (facts.afterlife.IsKnown) "я тут уже была!" {
                    action (doGoToWindow "я помню")
                }
            )
        }

    let w3 =
        window "коридор" {
            onEntry 
                ((fun s -> 
                    if (chars.World.CanStartActionInTheFlat s && not (chars.World.PoliceArriving.Get s)) then 
                        chars.World.PoliceArrive s 
                    else 
                        s)
                    >> (fun s -> 
                            if (chars.World.PoliceCameAlready s) then
                                withAnimation Warning s
                            else
                                s))

            ctxt
                (chars.World.PoliceCameAlready >> not)
                """Ты находишься в центре огромной комнаты, которая оформлена в современном стиле. Белые стены, белый потолок,
        большая белая двуспальная кровать... Комната залита ярким светом из панорамных окон. Из комнаты открыта дверь в прихожую."""
                """*!О боже! Кто-то неистово стучит в дверь и требует ее открыть.!* Похоже, придется подчиниться требованию, иначе они просто 
                вынесут дверь."""

            var (
                hidden (ands chars.World.PoliceCameAlready (chars.World.talkedToPolice.Get >> not)) "открыть дверь" {
                    action (doPushWindow "police.initflat")
                }
            )
            var (
                hidden (chars.World.talkedToPolice.Get) "к полицейським" {
                    action (doPushWindow "police.initflat")
                }
            )

            var ("пойти в санузел" --- "санузел.init")

            var (
                variant "" {
                    text "пойти на кухню"
                    action (doCond (chars.World.DeadBodyFoundThisLife.Get >> not)
                                (doPushWindow "кухня.шок")
                                (doPushWindow "кухня.init"))
                }
            )

            var (
                hidden (fun s -> facts.deadBodyFound.IsKnown s && (not <| chars.World.PoliceCameAlready s)) "" {
                    text "бежать через входную дверь"

                    action (
                        doCond
                            (chars.Myself.Naked.Get >> not)
                            (doPushWindow "снаружи.init")
                            (doGoToWindow "не могу выйти голая")
                    )

                    modify (facts.deadBodyFound.Acquire)
                }
            )

            var ("посмотреть в окно" --- "окна.init")
            var ("порыться в шкафах" --- "шкаф.init")
        }

    location "kitchen" {
        stxt "надо же, мы на кухне блять нахуй, я ебал"
        var (popVariant "блять, назад, поп нахуй")
        var (variant "нихуя" {
            changeLoc "kitchen"
        })
        var (popLocVariant "в пизду нахуй")
        var (pushLocVariant "еще один уровень рекурсии нахуй, погнали" "kitchen")
        locTarget "снова умираем нахуй" "никуда"
    } |> ignore

    createDialog
        "init"
        [ wInit
          w2
          w3
          window "я жива" {
              stxt "Я что... жива? Я спала? Это был сон? Мне приснился тот труп на кухне, мне приснилась... ауч, умирать больно. Это было так реально..."

              var ("да ладно" -- "2")
          } 
          window "я помню" {
              stxt "Я помню эту комнату... эту кровать... где моя одежда, кто я... Это все еще сон? А тело... Где тело?"

              var ("прожить все заново" -- "коридор")
              var (variant "рвануть на кухню" {
                  action (doMoveWithStack "кухня.шок" [ "init.коридор" ])
              })
          } 
          window "не могу выйти голая" {
              stxt "Я не могу выйти без одежды"

              var (
                  locked "хз где" (facts.shkafSeen.IsKnown) "" {
                      text "найти хоть какую-то одежду"
                      action (doMoveWithStack "шкаф.init" [ "init.коридор" ])
                  }
              )

              var (variant "отказаться от этой идеи" { action (doGoToWindow "коридор") })
          } ]
    |> ignore

    createDialog
        "окна"
        [ window "init" {
            stxt
                """Похоже, это где-то 25й этаж. Вниз очень высоко, но вокруг есть дома и выше. Не смотря на туман,
            тебе удалось разглядеть закрытые детские площадки и вход в подземный паркинг. Сквозь туман ярко пробивается
            вывеска салона красоты "Заура" """

            var ("открыть окно" -- "откр")
            var ("отойти от окна" -- "отойти")
          }
          window "откр" {
              stxt
                  """Ты открыла окно. Прохладный воздух нежно окутал твое обнаженное тело.
            Теперь его прекрасно видно с каждого окна окружающих высоток."""

              var (
                  variant "" {
                      text "заорать"

                      action (
                          doCond (facts.deadBodyFound.IsKnown >> not) (doPushWindow "utils.не буду") (doGoToWindow "заорать")
                      )
                  }
              )

              var ("станцевать" --- "utils.не буду")
              var ("закрыть окно" -- "init")
              var (hidden (chars.Myself.IphoneTaken.Get) "выбросить телефон в окно" { action (doGoToWindow "выбросить") })
          }
          window "заорать" {
              onEntry (facts.screamedInWindow.Acquire)
              stxt "Ты орешь в окно. Отлично. Теперь тебя точно видели все. Апплодисменты."
              var (popVariant "закрыть окно и отбежать, пока еще не поздно")
          }
          window "выбросить" {
              onEntry (
                  facts.thrownIphone.Acquire
                  >> chars.Myself.IphoneTaken.Set false
              )

              stxt
                  """Бросок... удар... звук разбитого стекла... сигнализация... твою мать, я только что разбила чужую тачку
            этим сраным телефоном! Жаль, что в жизни нет кнопки "отмена". Теперь я точно обратила на себя внимание.
            """

              var (popVariant "бежать от окна")
          }
          window "отойти" {
              stxt "Такое чувство, как будто кто-то наблюдает за мной"
              var (popVariant "отойти")
          } ]
    |> ignore

    createDialog
        "санузел"
        [ window "init" {
            stxt
                """Обычный унитаз, обычная душевая кабина. Ты неплохо выглядишь в зеркале. Особенно без одежды.
            """

            var (
                variant "" {
                    text "выйти из санузла"
                    action doPop
                }
            )

            var (
                variant "" {
                    text "воспользоваться туалетом"

                    action (
                        { DoOnce = { Target = "туалетинг" }
                          DoOther =
                            { TargetRef = { D = "utils"; W = "не хочу" }
                              Mod = None }
                          Property = Props.BoolProperty("doToilet", false) }
                    )
                }
            )

            var ("осмотреть" -- "осмотр")
            var ("вымыть руки" -- "нетводы")
            var ("посмотреть на себя в зеркало" -- "зеркало")
          }
          window "зеркало" {
              ctxt
                  chars.Myself.Naked.Get
                  """Шикарное тело. Произведение исскуства. Тонкая талия, широкие бедра, ровные ноги. Только где моя одежда..."""
                  """Даже одетой я выгляжу просто непревзойденно"""

              var ("закончить" -- "init")
          }
          window "стругануть" {
              stxt "Буээээ... Вроде полегчало..."
              var ("умыться" -- "нетводы")
          }
          window "нетводы" {
              onEntry facts.noWater.Acquire

              stxt
                  "К сожалению, горячую воду отключили. И холодную тоже. Боль. Ни рук помыть, ни умыться. Даже в бачке унитаза вода не бесконечная."

              var ("жизнь полна страданий" -- "init")
          }
          window "туалетинг" {
              stxt "Сделано. Ничего необычного не произошло."

              var (
                  variant "" {
                      text "Отлично :)"
                      action (doGoToWindow "init")
                  }
              )
          }
          window "осмотр" {
              stxt
                  """Зубная щетка одна. Бритва мужская, одна штука. Гель для душа - один, он же и шампунь.
            Не нужно быть гением дедукции, чтобы осознать, что тут проживает одинокий мужчина. Или проживал.
            """

              var ("ясно" -- "init")

              var (
                  "гель для душа и шампунь в одном - ужасно"
                  -- "init"
              )
          } ]
    |> ignore

    createDialog
        "кухня"
        [ window "init" {
            stxt
                """Тело мужчины все еще лежит на полу, рядом со столом. Остальное не имеет значения.
            """

            var (popVariant "уйти с кухни")
            var ("осмотреть тело мужчины" -- "осмотр тела")
            var ("осмотреть кухню" -- "осмотр1")
          }
          window "осмотр1" {
              stxt
                  """Дорогая немецкая бытовая техника. Выглядит так, как будто ею никто не пользовался ни разу в жизни.
                  Похоже, местные жители не особо любят готовить дома. Раковина пуста, все ножи, кроме того,
                  что торчит из мужчины, стоят на подставке для ножей. Очень аккуратно. Идеальная чистота, как
                  будто это номер отеля, который только что подготовили для нового клиента.
            """

              var ("открыть холодильник" -- "осмотр2")
          }
          window "осмотр2" {
              stxt
                  """Если тут однажды повесилась мышь - то ее давно уже съели. И, видимо, запили. Несколько бутылок с вином - 
                  это все, что нужно человеку для счастья. Судя по такому холодильнику.
            """

              var ("закрыть холодильник" -- "init")
          }
          window "шок" {
              stxt
                  """Кухня тоже оформлена в белых тонах... Стоп, что это? На полу лежит тело мужчины. Он одет в белую
            рубашку и строгие черные брюки, а то, что он больше не мужчина, а лишь тело - выдает торчащий из его груди нож,
            и лужа крови, растекшаяся под ним. Нет смысла подходить ближе, чтобы понять, что он мертв.
            """

              var ("что тут происходит?" -- "шок2")
          }
          window "шок2" {
              onEntry (facts.deadBodyFound.Acquire >> chars.World.DeadBodyFoundThisLife.Set true)
              stxt
                  """Мужчина убит. В этом сомнений нет. От вида окровавленного тела внутри тебя просыпается животный страх.
            Ты начинаешь бояться за свою жизнь. Вдруг тот, кто это сделал - все еще здесь?
            """

              var ("попытаться прийти в себя" -- "init")
              var ("запаниковать" -- "паника")
          }
          window "паника" {
              stxt
                  """ААААААААААААААААААААААААААААААААА
            """

              var (popVariant "выскочить из кухни")
              var ("АААААААААААААААААААААА" -- "паника")

              var (
                  variant "" {
                      text "стругануть"
                      action (doMoveWithStack "санузел.стругануть" [ "init.коридор" ])
                  }
              )
          }
          window "осмотр тела" {
              stxt "Я определенно не знакома с ним... или знакома..."
              var ("кто я вообще такая?" --- "осмотр.монолог")
              var ("орудие убийства" --- "осмотр.орудие")
              var ("одежда" --- "осмотр.одежда")
              var ("личность" --- "осмотр.личность")
              var ("обшарить карманы" --- "осмотр.карманы")

              var (
                  variant "" {
                      text "не могу на это смотреть больше"
                      action (doMoveWithStack "санузел.стругануть" [ "init.коридор" ])
                  }
              )

              var (
                  variant "" {
                      text "отойти от тела"
                      action (doGoToWindow "init")
                  }
              )
          } ]
    |> ignore

    createDialog
        "шкаф"
        [ window "init" {
            stxt
                "Огромный шкаф-купе с зеркалом. В зеркале отражаешься ты. Тебе это нравится, но думать об этом некогда."

            var (popVariant "уйти")
            var (hidden (fun x -> chars.Myself.Naked.Get x && facts.shkafSeen.IsKnown x) "надеть спортивную форму" {
                modify (chars.Myself.WearSport)
                action (doGoToWindow "одежда")
            })
            var (hidden (fun x -> chars.Myself.Naked.Get x && facts.shkafSeen.IsKnown x) "надеть мужской костюм" {
                modify (chars.Myself.WearOffice)
                action (doGoToWindow "одежда")
            })
            var (hidden (fun x -> chars.Myself.Naked.Get x |> not) "раздеться" {
                modify (chars.Myself.GetNaked)
                action (doGoToWindow "разделась")
            })

            var (
                variant "" {
                    text "заглянуть внутрь"
                    action (doCond (facts.shkafSeen.IsKnown >> not) (doGoToWindow "осмотр") (doGoToWindow "ничего"))
                }
            )
          }
          window "одежда" {
                stxt
                    "Наконец-то хоть какая-то одежда. Да, немного оверсайз. Раза в два. Но выбор у меня небольшой. Может, найду еще свою."
                var (popVariant "я все равно неплохо выгляжу в зеркале")
          }
          window "разделась" {
                stxt
                    "Ну вот, я снова в том, в чем мать родила. Где же моя одежда? Я вообще ношу одежду? Хотя мне идет и без нее."
                var (popVariant "мне нравится")
                var ("попробую другую одежду" -- "init")
          }
          window "ничего" {
              stxt "Никаких следов моей одежды тут нет, а вот у владельца шкафа определенно есть стиль."
              var (popVariant "понятно")
          }
          window "так уж и быть" {
              rand [ "Так уж и быть"
                     "Что поделать"
                     "Попытка - не пытка"
                     "Стоило попробовать" ]

              var (
                  variant "" {
                      text "бросить поиски"
                      action (doGoToWindow "init")
                      modify facts.shkafSeen.Acquire
                  }
              )
          }
          window "осмотр" {
              stxt
                  """На вешалке висят дорогие рубашки в строгом стиле. Там же видно несколько черных пиджаков.
            Несомненно, дорогих. На меня они определенно великоваты."""

              var ("осмотреть ящики" -- "ящики")
              var ("закрыть шкаф, безнадежно это" -- "init")
          }
          window "ящики" {
              stxt """Трусы. Просто мужские трусы с дебильным принтом. Точно не мои."""
              var ("осмотреть ящики дальше" -- "носки")

              var (
                  "закрыть уже шкаф, безнадежно это совсем"
                  -- "init"
              )

              var (popVariant "уйти от шкафа в отчаянии")
          }
          window "носки" {
              stxt
                  """Носки. Обычные носки. А вот еще носки с уточками. Видимо, ничего я тут не найду. Майки вот еще. Спотивный костюм... хм..."""

              var (
                  "стоит таки поискать СВОЮ одежду"
                  -- "так уж и быть"
              )
          } ]
    |> ignore

    createDialog
        "utils"
        [ window "не хочу" {
            stxt "Я не хочу делать этого сейчас"

            var (
                variant "" {
                    text "ясно"
                    action doPop
                }
            )
          }
          window "нема" {
              stxt "[ То, что находится дальше, скрыто под завесой мрака. ]"

              var (
                  variant "" {
                      text "вернусь сюда позже, когда это будет сделано разработчиком"
                      action doPop
                  }
              )
          }
          window "не могу" {
              stxt "Я не могу сделать это сейчас"

              var (
                  variant "" {
                      text "понятно"
                      action doPop
                  }
              )
          }
          window "не буду" {
              stxt "Я не буду делать этого сейчас"

              var (
                  variant "" {
                      text "ладно"
                      action doPop
                  }
              )
          } ]
    |> ignore

    GameChapter2.init (facts, chars) |> ignore
    GameChapter3.init (facts, chars) |> ignore

type GameDvaRunner() =
    interface Engine.IGameRunner with
        member _.InitialState() = INITIAL_STATE
        member _.Build() = build ()

let init () =
    let runner = GameDvaRunner()
    Engine.setGameRunner runner

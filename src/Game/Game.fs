module Game

open Dialog
open StoryWriter
open State
open Data
open Props
open DSL

let reset () = 
    resetGlobal<Dialog> REPO_DIALOG
    resetAll ()

type Person(name) =
    member x.IsAlive = BoolProperty("person" -. name -. "isalive", false)
    member x.IsFucked = BoolProperty("person" -. name -. "isFucked", false)

let Lexi = Person("Lexi")

let initData(): Map<string, obj> =
    let bases = Map.ofList [
            ("hello.world", true :> obj)
            ("ur.moms.age", 42 :> obj)
            ("one.eternity", "too much" :> obj)
            ("person.Lexi.knowledge", [|"huioledge"|] :> obj)
        ]
    bases
    

let checkRefs () =
    let realRefs = dumpGlobalPairs<Dialog> REPO_DIALOG
                   |> List.collect 
                      (fun (k, v) -> v.DialogWindows |> Map.toList |> List.map 
                                                                      (fun x -> {D = k; W = (fst x)}))
    checkReferencesAgainst realRefs
       

let init () =
    reset ()
    let d = dialog "entry" {
        add (dialogWindow "first" "" (stxt "hehehe") (nextTo "старт"))
        text
            "старт" 
            "Я"
            "Как ты себя чувствуешь?"
            [
                makeUnlockedVariant "Отлично" (toWindow "отлично")
                makeUnlockedVariant "Так себе" (toWindow "таксебе")
                makeUnlockedVariant "Хреново, честно говоря" (toWindow "хреново")
            ]
        text
            "отлично" 
            "Я"
            "А не пошел бы ты нахрен, если тебе так хорошо?"
            [
                makeUnlockedVariant "Нет, не пошел" (toWindow "дерзкий")
                makeUnlockedVariant "Согласен, уже иду" (toWindow "таксебе")
                makeUnlockedVariant "А ну ка повтори" (toWindow "хреново")
            ]
        text 
            "хреново"
            "Я"
            "Ну теперь все нормально. Как и должно было быть. Чего еще стоило ожидать."
            [
                "Начать заново" -- "старт"
            ]
        text
            "дерзкий" 
            "Я"
            "Уууу сука, пизды получишь"
            [
                makeUnlockedVariant "Сам получишь" (toWindow "охуел")
                makeUnlockedVariant "Понял" (toWindow "хреново")
                makeUnlockedVariant "А ну ка повтори, сам сука" (toWindow "дерзкий")
            ]
        text 
            "охуел"
            "Я"
            "Ну поздравляю. Ты охуел."
            [
                "Начать заново" -- "старт"
                "А сейчас тебе самому пизда, машина" -- "вникуда"
            ]
        text
            "таксебе"
            "Я"
            "Че, приуныл? А это всего лишь тест."
            [
                "Мне стало лучше" -- "отлично"
                "Что ты мне тут втираешь, машина" -- "дерзкий"
                "Давай по новой, короче" -- "старт"
            ]
    }

    

    let variant1 = "просто нажми меня" --- "entry.старт"

    
    let list = [ 
        windowgen "huindow" {
            stxt "string"
            var variant1
            var (vargen "hello" {
                hello "huilo"
            })
        }
        windowgen "huindow2" {
            stxt "huing"
            var (vargen "welcome" {
                hello "huilo2"
            })
        }
    ]

    let huilogWindows = 
        Map.empty
        |> Map.add "helloworld" { 
            Name = "helloworld";
            Actor = "_teller";
            Text = ptxt "lublu *(ur.moms.age)* te*bya";
            Variants = s [ variant1 ]
            OnEntry = None }
        |> Map.add "helloworld2" list.[1]


    let huilog = {
                    Name = "huilog";
                    DialogWindows = huilogWindows;
                    DefaultWindow = None
                }
    Data.save<Dialog> REPO_DIALOG huilog.Name huilog
    
    // checkRefs ()

let staticnothing = init ()

let ENTRY_STATE =
    { UI = UiState.DialogMode({ Reference = { D = "huilog"; W = "helloworld" } })
      UIStack = []
      Previous = None
      KnownFacts = Set.empty
      Iteration = 0
      Data = initData()
      Error = None
      Log = [] }

let makeEntryState() =
    ENTRY_STATE
    |> Lexi.IsAlive.Set false
    |> Lexi.IsFucked.Set true

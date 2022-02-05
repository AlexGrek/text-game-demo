module NPC

open DSL
open Props
open Dialog
open Person

type BasicAnswers =
    { DontKnow: string list
      Shock: string list
      DontBelieve: string list
      DontCare: string list }

let DefaultBasicAnswers =
    { DontKnow =
        [ "Я ничего не знаю об этом"
          "Не слышал про такое"
          "Это правда вообще? Не ну знаю" ]
      Shock =
        [ "Твою ж мать, серьезно?"
          "Я в шоке"
          "Едрить-колотить... Что в мире происходит..." ]
      DontBelieve = [ "Я не верю, что такое вообще может быть" ]
      DontCare =
        [ "Мне это не интересно"
          "Как-то все равно"
          "Абсолютно пофиг"
          "Без интереса"
          "... и че? ..."
          "Я ничего не думаю об этом"
          "Меня это не волнует в принципе" ] }

type Reaction =
    | Do of Actions.IAction
    | Say of string
    | Talk of Dialog.DialogWindow
    | DontKnow
    | Shock
    | DontBelieve
    | DontCare

let TALKER_ROLE_ID = "talker"

type Talker(name: string, basicAnswers: BasicAnswers, defaultReaction: Reaction) =
    inherit RoleModel.Role(TALKER_ROLE_ID)
    let knownFactsProp =
        ListStringProperty.Personal name "knownFacts"

    member val KnownFactsProperty = knownFactsProp

    member x.DoesKnow fact state =
        x.KnownFactsProperty.Contains fact state

    member val BasicAnswers = basicAnswers
    member val Name = name
    member val DefaultReaction = defaultReaction
    member x.ToActor = name

let private sWindow (a: Talker) name text exitButton =
    window name {
        stxt text
        actor (a.ToActor)
        var (popVariant exitButton)
    }

let private randTextWindow (a: Talker) name textOptions exitButton =
    window name {
        rand textOptions
        actor (a.ToActor)
        var (popVariant exitButton)
    }

let createAskAboutDialog (publicName: string) (talker: Talker) (ans: Map<string, Reaction>) =
    let dialogName = talker.Name + "_ask"

    let generateAvailableFacts =
        let createFactVariant (fid: string) =
            let reaction =
                if (Map.containsKey fid ans) then
                    ans.[fid]
                else
                    talker.DefaultReaction

            let pushToWindow target =
                (pushWindowRef { D = dialogName; W = target }) :> Actions.IAction

            let windowGo wName basics exButton =
                (pushToWindow wName, Some(randTextWindow talker wName basics exButton))

            let reactAction, reactWindow =
                match reaction with
                | Do (iaction) -> (iaction, None)
                | Say (text) -> (pushToWindow fid, Some(sWindow talker fid text "понятно"))
                | Talk (window) -> (pushToWindow window.Name, Some(window))
                | DontKnow -> windowGo "dontknow" talker.BasicAnswers.DontKnow "понятно"
                | Shock -> windowGo "shock" talker.BasicAnswers.Shock "ясно"
                | DontBelieve -> windowGo "dontbelieve" talker.BasicAnswers.DontBelieve "эх"
                | DontCare -> windowGo "dontCare" talker.BasicAnswers.DontCare "ладно"

            let fact = Facts.lookupFact fid

            let var =
                hidden (fact.IsKnown) fact.Name { action reactAction }

            (var, reactWindow)

        let facts = Facts.listAllFacts ()
        List.map createFactVariant facts

    let variants = (List.map fst generateAvailableFacts)
    let endDialog = popVariant "назад"

    let initialDialog =
        { DialogWindow.Name = "init"
          Actor = None
          Text =
            (stxt
             <| "Спросить, что " + publicName + " думает о...")
          Variants = s (endDialog :: variants)
          OnEntry = None }

    let ws =
        initialDialog
        :: (List.choose snd generateAvailableFacts)

    let dialog = createDialog dialogName ws
    dialogName + ".init"

let asTalker(p: Person) =
    (p.Roles().As TALKER_ROLE_ID) :?> Talker

type npcBuilder(person: Person) = 
    
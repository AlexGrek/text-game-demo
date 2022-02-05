module NPC

open DSL
open Props
open Dialog
open Person
open State
open PersonHub
open Actions

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

let createAskAboutDialog (sysName: string) (talker: Talker) (ans: Map<string, Reaction>) =
    let dialogName = sysName + "_ask"

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
                | Talk (window) -> (pushToWindow <| window.GetName(), Some(window))
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
        { TextDialogWindow.Name = "init"
          Actor = None
          Text =
            (stxt
             <| "Спросить, что " + talker.Name + " думает о...")
          Variants = s (endDialog :: variants)
          OnEntry = None }

    let ws =
        TextWindow(initialDialog)
        :: (List.choose snd generateAvailableFacts)

    let dialog = createDialog dialogName ws
    dialogName + ".init"

let asTalker (p: Person) =
    (p.Roles().As TALKER_ROLE_ID) :?> Talker

type NpcBuilderState =
    { SystemName: string
      DisplayName: State -> string
      FactReactions: Map<string, Reaction>
      Variants: State -> DialogVariant list
      StaticVariants: DialogVariant list
      ItemGivenReactions: Map<string, Reaction>
      Description: State -> RichText.RichText }
    member x.Build person =
        let talker = asTalker person

        let askAbout =
            createAskAboutDialog x.SystemName talker x.FactReactions

        { Name = x.SystemName
          Description = x.Description
          Design = HubDesign.defaultDesign
          FactsDialogLink = askAbout
          Variants =
            (fun s ->
                x.Variants s
                @ x.StaticVariants
                  @ (List.singleton (popVariant "уйти"))) }
        |> Data.save<PersonHub> REPO_PERSON_HUBS x.SystemName


type npcBuilder(person: Person) =
    let name =
        let talker = asTalker person
        talker.Name

    member __.Yield(_) : NpcBuilderState =
        { SystemName = person.Name
          DisplayName = s name
          FactReactions = Map.empty
          ItemGivenReactions = Map.empty
          Variants = s []
          StaticVariants = []
          Description = stxt name }

    [<CustomOperation("name")>]
    member __.Name(nbs: NpcBuilderState, name: State -> string) = { nbs with DisplayName = name }

    [<CustomOperation("fact")>]
    member __.Fact(nbs: NpcBuilderState, name: string, reaction: Reaction) =
        { nbs with FactReactions = nbs.FactReactions.Add(name, reaction) }

    [<CustomOperation("fact")>]
    member __.Fact(nbs: NpcBuilderState, fact: Facts.Fact, reaction: Reaction) =
        { nbs with FactReactions = nbs.FactReactions.Add(fact.FactId, reaction) }

    [<CustomOperation("itemGiven")>]
    member __.ItemGiven(nbs: NpcBuilderState, name: string, reaction: Reaction) =
        { nbs with FactReactions = nbs.ItemGivenReactions.Add(name, reaction) }

    [<CustomOperation("stxt")>]
    member __.Stxt(loc: NpcBuilderState, text: string) : NpcBuilderState = { loc with Description = stxt text }

    [<CustomOperation("ptxt")>]
    member __.Ptxt(loc: NpcBuilderState, text: string) : NpcBuilderState = { loc with Description = ptxt text }

    [<CustomOperation("ctxt")>]
    member __.Ctxt(loc: NpcBuilderState, predicate: State -> bool, text1: string, text2: string) : NpcBuilderState =
        { loc with Description = ctxt predicate text1 text2 }

    [<CustomOperation("var")>]
    member __.Var(loc: NpcBuilderState, variant: DialogVariant) =
        { loc with StaticVariants = variant :: loc.StaticVariants }

    [<CustomOperation("variants")>]
    member __.Vars(loc: NpcBuilderState, variants: State -> DialogVariant list) = { loc with Variants = variants }

    member __.Run(loc: NpcBuilderState) =
        printfn "initializing NPC %s" loc.SystemName
        loc.Build person

let npc person = npcBuilder person

let pushNpcDialog target =
    { PersonHubRef = target; PushPersonDialog.Mod = None; SpecificAction = None }

let npcDialogVariant text target =
    makeUnlockedVariant text (pushNpcDialog target)
module NPC

open DSL
open Props
open Dialog
open Person
open State
open PersonHub
open Actions
open LocationHub

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

let REPO_DISPLAY_NAMES_MAPPING = Data.GlobalRepository<State -> string>()

let saveDisplayNameMapping personID mapping =
    Data.save<State -> string> REPO_DISPLAY_NAMES_MAPPING personID mapping |> ignore

type NpcBuilderState =
    { SystemName: string
      DisplayName: State -> string
      FactReactions: Map<string, Reaction>
      Variants: State -> DialogVariant list
      StaticVariants: DialogVariant list
      ItemGivenReactions: Map<string, Reaction>
      Allowed: State -> AllowedInteractions
      Description: State -> RichText.RichText }
    member x.Build person =
        let talker = asTalker person

        let askAbout =
            createAskAboutDialog x.SystemName talker x.FactReactions
        
        saveDisplayNameMapping x.SystemName x.DisplayName

        { Name = x.SystemName
          Description = x.Description
          Design = HubDesign.defaultDesign
          FactsDialogLink = askAbout
          Allowed = x.Allowed
          Variants =
            (fun s ->
                x.Variants s
                @ x.StaticVariants
                  @ (List.singleton (popVariant "закончить разговор"))) }
        |> Data.save<PersonHub> REPO_PERSON_HUBS x.SystemName


type npcBuilder(person: Person) =
    let name =
        let talker = asTalker person
        talker.Name

    member __.Yield(_) : NpcBuilderState =
        { SystemName = person.Name
          DisplayName = s person.DefaultDisplayName
          FactReactions = Map.empty
          ItemGivenReactions = Map.empty
          Allowed = s AllowedInteractions.All
          Variants = s []
          StaticVariants = []
          Description = stxt name }

    [<CustomOperation("name")>]
    member __.Name(nbs: NpcBuilderState, name: State -> string) = { nbs with DisplayName = name }

    [<CustomOperation("staticname")>]
    member __.Name(nbs: NpcBuilderState, name: string) = { nbs with DisplayName = s name }

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

    [<CustomOperation("allow")>]
    member __.Allow(loc: NpcBuilderState, a: State -> AllowedInteractions) = 
        { loc with Allowed = a }

    [<CustomOperation("allow")>]
    member __.Allow(loc: NpcBuilderState, a: AllowedInteractions) = 
        { loc with Allowed = s a }

    member __.Run(loc: NpcBuilderState) =
        printfn "initializing NPC %s" loc.SystemName
        loc.Build person

let npc person = npcBuilder person

let doPushNpcDialog target =
    { PersonHubRef = target; PushPersonDialog.Mod = None; SpecificAction = None } :> IAction

let doPushNpcDialogSpecific targetHub targetDialog =
    { PersonHubRef = targetHub; PushPersonDialog.Mod = None; SpecificAction = Some(doPushWindow targetDialog) } :> IAction

let doPushNpcDialogAction targetHub action =
    { PersonHubRef = targetHub; PushPersonDialog.Mod = None; SpecificAction = Some(action) } :> IAction

let npcDialogVariant text (target: Person) =
    makeUnlockedVariant text (doPushNpcDialog target.Name)

let npcDialogSpecificVariant text (target: Person) spec =
    makeUnlockedVariant text (doPushNpcDialogSpecific target.Name spec)

let npcDialogActionVariant text (target: Person) spec =
    makeUnlockedVariant text (doPushNpcDialogAction target.Name spec)

let findDisplayName name (s: State) =
    if (REPO_DISPLAY_NAMES_MAPPING.ContainsKey name) then
        Some(Data.getGlobal<Person> REPO_PERSONS name, s |> Data.getGlobal<State -> string> REPO_DISPLAY_NAMES_MAPPING name)
    else
        None

// LOCATION creation is also part of NPC engine

let getPeopleOnLocation name (state: State) =
    let createForPerson (p: Person) =
        let name = 
            match (findDisplayName p.Name state) with
            | Some(_, name) -> name
            | None -> p.DisplayName state
        { Pic = None
          Variant =
            makeUnlockedVariant
                name
                (doPushNpcDialog p.Name) }

    let inLocation (l: InLocation) = (l.CurrentLocation.Get state) = name

    let checkPerson (p: Person) =
        match (asInLocation p) with
        | Some (loc) -> inLocation loc
        | None -> false

    Data.values REPO_PERSONS
    |> Seq.filter checkPerson
    |> Seq.toList
    |> List.map createForPerson

type LocationHubStaticVariants =
    { LocationHub: LocationHub
      Variants: DialogVariant list
      StaticPersons: LocationHubVariant list }
    member x.Build() =
        { x.LocationHub with
            Variants = s (List.rev x.Variants)
            // include both static persons and dynamic persons
            Persons =
                fun state ->
                    (List.rev x.StaticPersons)
                    @ (x.LocationHub.Persons state) }


type LocationHubBuilder(name: string) =
    let initialLocation =
        { Locations = []
          Design = HubDesign.defaultDesign
          Persons = getPeopleOnLocation name
          Variants = s []
          Description = stxt ""
          Name = name }

    member __.Yield(_) : LocationHubStaticVariants =
        { LocationHub = initialLocation
          Variants = []
          StaticPersons = [] }

    member __.Run(a: LocationHubStaticVariants) : LocationHub = a.Build() |> Data.save REPO_LOCATIONS name

    [<CustomOperation("locVariant")>]
    member __.LocVar(loc: LocationHubStaticVariants, variant: DialogVariant) : LocationHubStaticVariants =
        { loc with
            LocationHub =
                { loc.LocationHub with
                    Locations =
                        List.rev (
                            (makePicturelessLocationVariant variant)
                            :: loc.LocationHub.Locations
                        ) } }

    [<CustomOperation("design")>]
    member __.Des(loc: LocationHubStaticVariants, design: HubDesign.HubDesign) : LocationHubStaticVariants =
        { loc with LocationHub = { loc.LocationHub with Design = design } }


    [<CustomOperation("locTarget")>]
    member __.LocVar(loc: LocationHubStaticVariants, toName: string, target: string) : LocationHubStaticVariants =
        let variant = variant toName { pushLoc target }

        { loc with
            LocationHub =
                { loc.LocationHub with
                    Locations =
                        List.rev (
                            (makePicturelessLocationVariant variant)
                            :: loc.LocationHub.Locations
                        ) } }

    [<CustomOperation("personVariant")>]
    member __.PersVar(loc: LocationHubStaticVariants, variant: DialogVariant) : LocationHubStaticVariants =
        { loc with
            StaticPersons =
                (makePicturelessLocationVariant variant)
                :: loc.StaticPersons }


    [<CustomOperation("stxt")>]
    member __.Stxt(loc: LocationHubStaticVariants, text: string) : LocationHubStaticVariants =
        { loc with LocationHub = { loc.LocationHub with Description = stxt text } }

    [<CustomOperation("ptxt")>]
    member __.Ptxt(loc: LocationHubStaticVariants, text: string) : LocationHubStaticVariants =
        { loc with LocationHub = { loc.LocationHub with Description = ptxt text } }

    [<CustomOperation("ctxt")>]
    member __.Ctxt
        (
            loc: LocationHubStaticVariants,
            predicate: State -> bool,
            text1: string,
            text2: string
        ) : LocationHubStaticVariants =
        { loc with LocationHub = { loc.LocationHub with Description = ctxt predicate text1 text2 } }

    [<CustomOperation("var")>]
    member __.Var(loc: LocationHubStaticVariants, variant: DialogVariant) : LocationHubStaticVariants =
        { loc with Variants = variant :: loc.Variants }

let location name = LocationHubBuilder name

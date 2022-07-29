module NPC

open DSL
open Props
open Dialog
open Person
open State
open PersonHub
open Actions
open LocationHub

type PersonConfiguration = {
        Person: Person
        NPC: State -> string
        DisplayName: State -> string
        }

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
        ListStringProperty.Personal name "known_facts"

    member val KnownFactsProperty = knownFactsProp
    member val CurrentNpc =
        StringProperty.Personal name "current_npc_id" ""

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
                printfn "map: %A" ans
                printfn "fid: %s" fid
                printfn "map contains key: %A" <| Map.containsKey fid ans
                printfn "talker: %A" talker
                if (Map.containsKey fid ans) then
                    ans.[fid]
                else
                    talker.DefaultReaction

            
            printfn "reaction: %A" reaction

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
             <| "Поговорить о...")
          Variants = s (endDialog :: variants)
          OnEntry = None }

    let ws =
        TextWindow(initialDialog)
        :: (List.choose snd generateAvailableFacts)

    let dialog = createDialog dialogName ws
    dialogName + ".init"

let asTalker (p: Person) =
    (p.Roles().As TALKER_ROLE_ID) :?> Talker

let REPO_PERSON_CONFIGS = 
    Data.GlobalRepository<PersonConfiguration>()

let savePersonConfig personID =
    Data.save<PersonConfiguration> REPO_PERSON_CONFIGS personID
    >> ignore

type NpcBuilderState =
    { SystemName: string
      DialogName: string
      FactReactions: Map<string, Reaction>
      Variants: State -> DialogVariant list
      StaticVariants: DialogVariant list
      ItemGivenReactions: Map<string, Reaction>
      StartingDialog: State -> Actions.Jump option
      Allowed: State -> AllowedInteractions
      ExitVariant: DialogVariant;
      Description: State -> RichText.RichText }
    member x.Build person =
        let talker = asTalker person

        let dialogName =
            x.SystemName + "@" + x.DialogName

        let askAbout =
            createAskAboutDialog dialogName talker x.FactReactions

        { Name = dialogName
          Description = x.Description
          Design = HubDesign.defaultDesign
          FactsDialogLink = askAbout
          StartingDialog = x.StartingDialog
          Allowed = x.Allowed
          ExitVariant = x.ExitVariant
          Variants = (fun s -> x.Variants s @ x.StaticVariants) }
        |> Data.save<PersonHub> REPO_PERSON_HUBS dialogName


type npcBuilder(person: Person, dialogName) =
    let name =
        let talker = asTalker person
        talker.Name

    member __.Yield(_) : NpcBuilderState =
        { SystemName = person.Name
          DialogName = dialogName
          FactReactions = Map.empty
          ItemGivenReactions = Map.empty
          Allowed = s AllowedInteractions.All
          Variants = s []
          StaticVariants = []
          StartingDialog = s None
          ExitVariant = (popVariant "закончить разговор")
          Description = stxt name }

    [<CustomOperation("exittext")>]
    member __.exitVariantName(nbs: NpcBuilderState, name: string) = { nbs with ExitVariant = (popVariant name) }

    [<CustomOperation("exit")>]
    member __.exitVariant(nbs: NpcBuilderState, ex: DialogVariant) = { nbs with ExitVariant = ex }

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
    member __.Allow(loc: NpcBuilderState, a: State -> AllowedInteractions) = { loc with Allowed = a }

    [<CustomOperation("allow")>]
    member __.Allow(loc: NpcBuilderState, a: AllowedInteractions) = { loc with Allowed = s a }

    [<CustomOperation("start")>]
    member __.Strt(loc: NpcBuilderState, d: State -> Actions.Jump option) = { loc with StartingDialog = d }

    [<CustomOperation("startonce")>]
    member __.StrtOnce(loc: NpcBuilderState, targetLink: string) =
        let reference = Data.UReference.Parse targetLink

        let jump =
            { Actions.Jump.TargetRef = reference
              Mod = Some(meetPerson person) }

        { loc with
            StartingDialog =
                (fun s ->
                    if (doesKnowPerson person s) then
                        None
                    else
                        Some(jump)) }

    member __.Run(loc: NpcBuilderState) =
        loc.Build person

let npc person dialogName = npcBuilder (person, dialogName)

let doPushNpcDialog (targetNPC: string) =
    { PersonHubRef = targetNPC
      PushPersonDialog.Mod = None
      SpecificAction = None }
    :> IAction

let doFindNpcDialog personName =
    let lookupConfig personName =
        Data.getOrNone REPO_PERSON_CONFIGS personName

    let lookupAllNpcs personName =
        let lst = 
            let key = personName + "@"
            Data.dumpGlobalKeys REPO_PERSON_HUBS
            |> List.filter (fun (s: string) -> s.StartsWith(key))
        if List.isEmpty lst then
            failwithf "Cannot find at least one NPC or PERSON_CONFIG for person '%s'" personName
        if List.length lst > 1 then
            printfn "WARNING: multiple NPC found for person '%s', please specify one" personName
        lst

    let findPersonNpc state =
        let person = Data.getGlobal REPO_PERSONS personName
        let talker = asTalker person
        let currentNpcId = talker.CurrentNpc.Get state
        if currentNpcId = "" then
            match (lookupConfig personName) with
            | Some(config) ->
                printfn "Using person config for '%s' to find NPC" personName 
                config.NPC state
            | None ->
                printfn "Using global NPC lookup to find NPC for '%s'" personName
                List.head (lookupAllNpcs personName)
        else
            printfn "Using Talker.CurrentNpc to find NPC for '%s'" personName
            currentNpcId

    { Person = personName; PersonDecider.Decider = findPersonNpc; Mod = None; SpecificAction = None }

let doPushNpcDialogSpecific targetHub targetDialog =
    { PersonHubRef = targetHub
      PushPersonDialog.Mod = None
      SpecificAction = Some(doPushWindow targetDialog) }
    :> IAction

let doPushNpcDialogAction targetHub action =
    { PersonHubRef = targetHub
      PushPersonDialog.Mod = None
      SpecificAction = Some(action) }
    :> IAction

let npcDialogVariant text (target: Person) =
    makeUnlockedVariant text (doFindNpcDialog target.Name)

let npcDialogSpecificVariant text (target: Person) spec =
    makeUnlockedVariant text (doPushNpcDialogSpecific target.Name spec)

let npcDialogActionVariant text (target: Person) spec =
    makeUnlockedVariant text (doPushNpcDialogAction target.Name spec)

let findDisplayName name (s: State) =
    if (REPO_PERSON_CONFIGS.ContainsKey name) then
        let config = 
            (Data.getGlobal<PersonConfiguration> REPO_PERSON_CONFIGS name)
        let displayName = config.DisplayName s
        Some(
             Data.getGlobal<Person> REPO_PERSONS name,
             displayName
        )
    else
        None

let mustFindDisplayName name state =
    match (findDisplayName name state) with
    | Some(name) -> name
    | None -> 
        let person = Data.getGlobal REPO_PERSONS name
        (person, person.DisplayName state)

// LOCATION creation is also part of NPC engine

let getPeopleOnLocation name (state: State) =
    let createForPerson (p: Person) =
        let name =
            match (findDisplayName p.Name state) with
            | Some (_, name) -> name
            | None -> p.DisplayName state

        { Pic = None
          Variant = makeUnlockedVariant name (doPushNpcDialog p.Name) }

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

    member __.Run(a: LocationHubStaticVariants) : LocationHub =
        a.Build() |> Data.save REPO_LOCATION_HUBS name

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

type BasicPerson(id, displayName, located) =
    inherit Person(id, displayName)
    override x.Roles() = 
        RoleModel.RoleModel([
            InLocation(x.Name, located)
            Talker(x.Name, DefaultBasicAnswers, DontCare)
        ])

type PersonConfigurationBuilder(person: Person) =
    let name = person.Name

    member __.Yield(_) : PersonConfiguration =
        { Person = person
          NPC = s ""
          DisplayName = s person.DefaultDisplayName }

    member __.Run(a: PersonConfiguration) : PersonConfiguration =
        Data.save<PersonConfiguration> REPO_PERSON_CONFIGS name a

    [<CustomOperation("name")>]
    member __.Name(p: PersonConfiguration, nameGenerator: State -> string) : PersonConfiguration =
        { p with DisplayName = nameGenerator }

    [<CustomOperation("hub")>]
    member __.Name(p: PersonConfiguration, npc: PersonHub) : PersonConfiguration =
        { p with NPC = s npc.Name }
    
    [<CustomOperation("hubName")>]
    member __.Name(p: PersonConfiguration, hubName: string) : PersonConfiguration =
        { p with NPC = s <| p.Person.Name + "@" + hubName }

let personConfig person = PersonConfigurationBuilder(person)

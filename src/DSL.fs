module DSL

open State
open Dialog
open Actions
open Data
open LocationHub

type StaticWindowGenerator =
    { Name: string
      Actor: string
      Text: State -> RichText.RichText
      StaticVariants: DialogVariant list
      OnEntry: Option<State -> State> }
    member x.Build() =
        { Name = x.Name
          Actor = x.Actor
          Text = x.Text
          Variants = s (List.rev x.StaticVariants)
          OnEntry = x.OnEntry }

type DialogVariantGen =
    { V: DialogVariant
      Modifiers: List<State -> State> }
    member x.Build() =
        match x.Modifiers with
        | [] -> x.V
        | _ -> { x.V with Action = List.fold (fun acc el -> acc.ComposeAfter el) x.V.Action x.Modifiers }

type WindowBuilder(name: string) =
    let staticDialogWindow name actor textGen variants onEntry =
        { Name = name
          Actor = actor
          Text = textGen
          StaticVariants = variants
          OnEntry = onEntry }

    member __.Yield(_) : StaticWindowGenerator =
        staticDialogWindow name "" (stxt "") [] None

    member __.Run(dialog: StaticWindowGenerator) : DialogWindow = dialog.Build()

    [<CustomOperation("stxt")>]
    member __.Stxt(dialog: StaticWindowGenerator, text: string) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor (stxt text) dialog.StaticVariants dialog.OnEntry

    [<CustomOperation("actor")>]
    member __.Actor(dialog: StaticWindowGenerator, actor: string) : StaticWindowGenerator =
        staticDialogWindow name actor dialog.Text dialog.StaticVariants dialog.OnEntry

    [<CustomOperation("rand")>]
    member __.RandomOf(dialog: StaticWindowGenerator, text: string list) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor (randomTextOf text) dialog.StaticVariants dialog.OnEntry

    [<CustomOperation("ctxt")>]
    member __.Ctxt
        (
            dialog: StaticWindowGenerator,
            predicate: State -> bool,
            text1: string,
            text2: string
        ) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor (ctxt predicate text1 text2) dialog.StaticVariants dialog.OnEntry

    [<CustomOperation("ptxt")>]
    member __.Ptxt(dialog: StaticWindowGenerator, text: string) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor (ptxt text) dialog.StaticVariants dialog.OnEntry

    [<CustomOperation("var")>]
    member __.Get2(dialog: StaticWindowGenerator, variant: DialogVariant) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor dialog.Text (variant :: dialog.StaticVariants) dialog.OnEntry

    [<CustomOperation("onEntry")>]
    member __.OnEntry(dialog: StaticWindowGenerator, action: State -> State) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor dialog.Text dialog.StaticVariants
        <| Some(action)

let window x = WindowBuilder(x)

type VariantBuilder(text: string, createVariant: string -> IAction -> DialogVariant) =
    member __.Yield(_) : DialogVariantGen =
        let variant = createVariant text <| NoAction()
        { V = variant; Modifiers = [] }

    member __.Run(v: DialogVariantGen) = v.Build()

    [<CustomOperation("action")>]
    member __.SetAction(v: DialogVariantGen, a: IAction) : DialogVariantGen = { v with V = createVariant v.V.Text a }

    [<CustomOperation("modify")>]
    member __.SetMod(v: DialogVariantGen, m: State -> State) : DialogVariantGen =
        { v with Modifiers = m :: v.Modifiers }

    [<CustomOperation("goto")>]
    member __.SetActionGoto(v: DialogVariantGen, a: string) : DialogVariantGen =
        { v with V = createVariant v.V.Text { Target = a } }

    [<CustomOperation("gotoOnce")>]
    member __.SetActionGotoOnce
        (
            v: DialogVariantGen,
            propertyName: string,
            gotoOnce: string,
            gotoOther: string
        ) : DialogVariantGen =
        { v with
            V =
                createVariant
                    v.V.Text
                    { DoOnce = { Target = gotoOnce }
                      DoOther = { Target = gotoOther }
                      Property = Props.BoolProperty("propertyName", false) } }


    [<CustomOperation("push")>]
    member __.SetActionGoto(v: DialogVariantGen, d: string, w: string) : DialogVariantGen =
        { v with
            V =
                makeUnlockedVariant
                    v.V.Text
                    { TargetRef = { D = d; W = w }
                      Mod = None } }

    [<CustomOperation("pushLoc")>]
    member __.SetActionPushLoc(v: DialogVariantGen, loc: string) : DialogVariantGen =
        { v with
            V =
                makeUnlockedVariant
                    v.V.Text
                    { LocationRef = loc
                      PushLocation.Mod = None } }

    [<CustomOperation("changeLoc")>]
    member __.SetActionChangeLoc(v: DialogVariantGen, loc: string) : DialogVariantGen =
        { v with
            V =
                makeUnlockedVariant
                    v.V.Text
                    { LocationRef = loc
                      ChangeLocation.Mod = None } }

    [<CustomOperation("text")>]
    member __.SetText(v: DialogVariantGen, s: string) : DialogVariantGen =
        { v with V = makeUnlockedVariant s v.V.Action }

let variant text =
    VariantBuilder(text, makeUnlockedVariant)

let locked reason predicate text =
    VariantBuilder(text, (makeLockedVariant reason predicate))

let hidden predicate text =
    VariantBuilder(text, makeHiddenVariant predicate)

let makeDialogWindowsFromList (lst: DialogWindow list) =
    List.map (fun (d: DialogWindow) -> (d.Name, d)) lst
    |> Map.ofList

let saveDialog (dialog: Dialog) =
    printfn "Saving dialog %A with windows %A" dialog.Name dialog.DialogWindows
    Data.save<Dialog> REPO_DIALOG dialog.Name dialog

let createDialog name windows =
    { Name = name
      DefaultWindow = None
      DialogWindows = makeDialogWindowsFromList windows }
    |> saveDialog


let inline (--) (x: string) (y: string) = makeUnlockedVariant x (toWindow y)

// let (---) (x: string) (y: UReference) =
//     makeUnlockedVariant x (pushWindow y)

let (---) (x: string) (y: string) =
    let reference = UReference.Parse y
    makeUnlockedVariant x (pushWindowRef reference)

// ACTIONS

let pop = { Pop.Mod = None }

let popLoc = { PopToLocation.Mod = None }

let pushWindow target =
    let reference = UReference.Parse target
    (pushWindowRef reference)

let pushLoc target = { LocationRef = target; PushLocation.Mod = None }

let changeLoc target = { LocationRef = target; ChangeLocation.Mod = None }

let once (propName: string) actionOnce actionOther =
    { Property = Props.BoolProperty("once::" + propName, false)
      DoOnce = actionOnce
      DoOther = actionOther }

let goToWindow target = { Target = target }

let popVariant text = makeUnlockedVariant text (pop)

let popLocVariant text = makeUnlockedVariant text (popLoc)

let pushLocVariant text target = makeUnlockedVariant text (pushLoc target)

let moveWithStackRef targetRef stackRef =
    { Target = targetRef
      Mod = None
      Stack = stackRef }

let moveWithStack target stack =
    moveWithStackRef (UReference.Parse target)
    <| List.map UReference.Parse stack

let cond p onTrue onFalse =
    { Predicate = p
      OnTrue = onTrue
      OnFalse = onFalse }

let getPeopleOnLocation name (state: State) = 
    let createForPerson (p: Person.Person) =
        {
            Pic = None
            Variant = 
                makeUnlockedVariant 
                    (p.DisplayName state)
                    { TargetRef = { D = p.Name; W = "init" }
                      Mod = None }
        }
    let inLocation (l: Person.InLocation) =
        (l.CurrentLocation.Get state) = name
    let checkPerson (p: Person.Person) =
        match (Person.asInLocation p) with
        | Some(loc) -> inLocation loc
        | None -> false
    values Person.REPO_PERSONS
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
            Persons = fun state -> (List.rev x.StaticPersons) @ (x.LocationHub.Persons state)
         }


type LocationHubBuilder(name: string) =
    let initialLocation =
        { Locations = []
          Persons = getPeopleOnLocation name
          Variants = s []
          Description = stxt ""
          Name = name }

    member __.Yield(_) : LocationHubStaticVariants =
        { LocationHub = initialLocation
          Variants = []
          StaticPersons = [] }

    member __.Run(a: LocationHubStaticVariants) : LocationHub = a.Build() |> save REPO_LOCATIONS name

    [<CustomOperation("locVariant")>]
    member __.LocVar(loc: LocationHubStaticVariants, variant: DialogVariant) : LocationHubStaticVariants =
        { loc with LocationHub =
                    { loc.LocationHub with
                        Locations =
                            List.rev ((makePicturelessLocationVariant variant) :: loc.LocationHub.Locations)}
        }

    
    [<CustomOperation("locTarget")>]
    member __.LocVar(loc: LocationHubStaticVariants, toName: string, target: string) : LocationHubStaticVariants =
        let variant = variant toName {
            pushLoc target
        }
        { loc with LocationHub =
                    { loc.LocationHub with
                        Locations =
                            List.rev ((makePicturelessLocationVariant variant) :: loc.LocationHub.Locations)}
        }

    [<CustomOperation("personVariant")>]
    member __.PersVar(loc: LocationHubStaticVariants, variant: DialogVariant) : LocationHubStaticVariants =
        { loc with StaticPersons =
                    (makePicturelessLocationVariant variant) :: loc.StaticPersons}


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
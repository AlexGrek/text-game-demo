module DSL

open State
open Dialog
open Actions
open Data
open LocationHub
open Person

type StaticWindowGenerator =
    { Name: string
      Actor: string option
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

type WindowBuilder(name: string, defaultActor: string option) =
    let staticDialogWindow name actor textGen variants onEntry =
        { Name = name
          Actor = actor
          Text = textGen
          StaticVariants = variants
          OnEntry = onEntry }

    member __.Yield(_) : StaticWindowGenerator =
        staticDialogWindow name defaultActor (stxt "") [] None

    member __.Run(dialog: StaticWindowGenerator) : DialogWindow = TextWindow(dialog.Build())

    [<CustomOperation("stxt")>]
    member __.Stxt(dialog: StaticWindowGenerator, text: string) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor (stxt text) dialog.StaticVariants dialog.OnEntry

    [<CustomOperation("actor")>]
    member __.Actor(dialog: StaticWindowGenerator, actor: string) : StaticWindowGenerator =
        staticDialogWindow name (Some(actor)) dialog.Text dialog.StaticVariants dialog.OnEntry

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

let window x = WindowBuilder(x, None)

let windowWithActor x act = WindowBuilder(x, Some(act))

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
    List.map (fun (d: DialogWindow) -> (d.GetName(), d)) lst
    |> Map.ofList

let saveDialog (dialog: Dialog) =
    // printfn "Saving dialog %A with windows %A" dialog.Name dialog.DialogWindows
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

let doPop = { Pop.Mod = None } :> IAction

let doPopLoc = { PopToLocation.Mod = None } :> IAction

let doPushWindow target =
    let reference = UReference.Parse target
    (pushWindowRef reference) :> IAction

let doPopPush target =
    let reference = UReference.Parse target
    (popPushWindowRef reference) :> IAction

let doPushLoc target =
    { LocationRef = target
      PushLocation.Mod = None } :> IAction

let doChangeLoc target =
    { LocationRef = target
      ChangeLocation.Mod = None } :> IAction

let doOnce (propName: string) actionOnce actionOther =
    { Property = Props.BoolProperty("once::" + propName, false)
      DoOnce = actionOnce
      DoOther = actionOther } :> IAction

let doGoToWindow target = { Target = target } :> IAction

let popVariant text = makeUnlockedVariant text (doPop)

let popLocVariant text = makeUnlockedVariant text (doPopLoc)

let pushLocVariant text target =
    makeUnlockedVariant text (doPushLoc target)

let doMoveWithStackRef targetRef stackRef =
    { Target = targetRef
      Mod = None
      Stack = stackRef } :> IAction

let doMoveWithStack target stack =
    doMoveWithStackRef (UReference.Parse target)
    <| List.map UReference.Parse stack

let doCond p onTrue onFalse =
    { Predicate = p
      OnTrue = onTrue
      OnFalse = onFalse } :> IAction

// was person met before or any fact about person known
let doesKnowPerson (p: Person) (s: State) = s.KnownPersons.ContainsKey <| p.Name

// make person known
let meetPerson (p: Person) s =
    if (doesKnowPerson p s) then
        s
    else
        { s with KnownPersons = s.KnownPersons.Add(p.Name, Set.empty) }

let proxyWindow name (proxyAction: State -> IAction) =
    Proxy(
        { ProxyWindow.Name = name
          Action = proxyAction }
    )

let doJump target =
    let reference = UReference.Parse target
    {Actions.Jump.TargetRef = reference; Mod = None}
    :> IAction
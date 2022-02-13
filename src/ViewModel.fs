module ViewModel

open RichText
open LocationHub
open PersonHub
open State
open Actions
open Person

type DialogActorView =
    | UnknownActor of string
    | RealActor of Person * string
    | NoActor
    with
      member x.asString() =
        match x with 
        | UnknownActor(a) -> Some(a)
        | NoActor -> None
        | RealActor(_, text) -> Some(text)

type DialogVariantView =
    { Text: string
      Action: IAction
      IsLocked: Dialog.DialogOptionLock }
    static member OfDialogVariant (s: State) (d: Dialog.DialogVariant) =
        { Text = d.Text
          Action = d.Action
          IsLocked = d.IsLocked s }
    static member MakeUnlockedVariant (t : string) (act: IAction) =
      {
        Text = t
        Action = act
        IsLocked = Dialog.Unlocked
      }

type LocationHubVariantView =
    { Pic: string option
      Variant: DialogVariantView }
    static member OfLocationHubVariant (s: State) (h: LocationHubVariant) =
        { Pic = h.Pic
          Variant = DialogVariantView.OfDialogVariant s h.Variant }

let filterOutVariants (getVariant: 'a -> DialogVariantView) (items: 'a list) =
    let predicate a =
        let variant = getVariant a

        match (variant.IsLocked) with
        | Dialog.Hidden -> false
        | _ -> true

    List.filter predicate items

let makeActor actor state =
  match actor with
  | None -> NoActor
  | Some(actorId) -> 
    match (NPC.findDisplayName actorId state) with
      | None -> UnknownActor(actorId)
      | Some(pers, text) -> RealActor(pers, text)

type DialogViewModel =
    { Actor: DialogActorView
      Text: RichText
      Variants: DialogVariantView list }
    static member OfDialogWindow (s: State) (dwindow: Dialog.DialogWindow) =
      match dwindow with
      | Dialog.TextWindow(d) ->
        { Actor = makeActor d.Actor s
          Text = d.Text s
          Variants =
            List.map (DialogVariantView.OfDialogVariant s) (d.Variants s)
            |> filterOutVariants id }
      | Dialog.Proxy(_) ->
          failwith "should never render proxy windows"

type LocationHubViewModel =
    { Text: RichText
      DisplayName: string
      Variants: DialogVariantView list
      Design: HubDesign.HubDesign
      Locations: LocationHubVariantView list
      Persons: LocationHubVariantView list }
    static member OfLocationHub (s: State) (d: LocationHub) =
      {
        Text = d.Description s
        DisplayName = d.Name
        Design = d.Design
        Variants = 
          List.map (DialogVariantView.OfDialogVariant s) (d.Variants s)
          |> filterOutVariants id
        Locations = 
          List.map (LocationHubVariantView.OfLocationHubVariant s) d.Locations
          |> filterOutVariants (fun (a: LocationHubVariantView) -> a.Variant)
        Persons =
          List.map (LocationHubVariantView.OfLocationHubVariant s) (d.Persons s)
          |> filterOutVariants (fun (a: LocationHubVariantView) -> a.Variant)
      }

let createAskAboutButton s d =
  let allowances = d.Allowed s
  if (allowances.AllowedTalkAbout) then
    (List.singleton <| DialogVariantView.MakeUnlockedVariant "спросить о..." (DSL.doPushWindow d.FactsDialogLink))
  else
    []

type SpecialPersonKey = 
  | Exit of DialogVariantView
  | Additional of DialogVariantView

type PersonHubViewModel =
    { Text: RichText
      DisplayName: string
      Design: HubDesign.HubDesign
      Variants: DialogVariantView list
      SpecialKeys: SpecialPersonKey list }
    static member OfPersonHub (s: State) (d: PersonHub) =
      {
        Text = d.Description s
        DisplayName = d.Name //TODO: change this to actual display name
        Design = d.Design
        SpecialKeys = 
          List.map (fun button -> Additional(button)) (createAskAboutButton s d)
          @ [ Exit(DialogVariantView.OfDialogVariant s d.ExitVariant) ]
        Variants =
          ((List.map (DialogVariantView.OfDialogVariant s) (d.Variants s)))
          |> filterOutVariants id
      }

type UIView =
    | DialogView of DialogViewModel
    | LocaitionHubView of LocationHubViewModel
    | PersonHubView of PersonHubViewModel
    static member OfUiState(s: State) =
        match s.UI with
        | DialogMode (dstate) ->
            let ref = dstate.Reference
            let dialog = Data.getGlobal Dialog.REPO_DIALOG ref.D
            let window = dialog.DialogWindows.[ref.W]
            DialogView(DialogViewModel.OfDialogWindow s window)
        | LocationHubMode(loc) ->
          let hub = Data.getGlobal REPO_LOCATIONS loc.LocReference
          LocaitionHubView(LocationHubViewModel.OfLocationHub s hub)
        | PersonHubMode(personHub) ->
          let hub = Data.getGlobal REPO_PERSON_HUBS personHub.PersonHubReference
          PersonHubView(PersonHubViewModel.OfPersonHub s hub)

let renderFacts (set: Set<string>) =
    let cast (el: string) = Data.getGlobal Facts.REPO_FACTS el
    Seq.map cast set |> Seq.toList

type UpdateMessage =
  | FactAccquired of Facts.Fact

let findNewFacts (old: Facts.Fact list) (news: Facts.Fact list) =
  let oldToMatch = List.map (fun {Facts.FactId = id} -> id) old
  let newFacts = List.filter (fun {Facts.FactId = fact} -> List.contains fact oldToMatch |> not) news
  printfn "New facts found: %A, searching in OLD={{%A}}; NEW={{%A}}" newFacts old news
  newFacts

type View =
    { Facts: Facts.Fact list
      Updates: UpdateMessage list
      UI: UIView }
    static member OfState(s: State) =
        { UI = UIView.OfUiState s
          Facts = renderFacts s.KnownFacts
          Updates = [] }

    member this.findUpdatesWithPrev (prev: View) =
        if (this.Facts.Length = prev.Facts.Length) then
          []
        else
          findNewFacts prev.Facts this.Facts
          |> List.map (fun x -> FactAccquired(x))

    static member OfStateWithPrev (s: State) (prev: View) =
        printfn "rendering with previous state"
        let current = View.OfState s
        let messages = current.findUpdatesWithPrev prev
        { current with Updates = messages }


type ViewOrError =
    | View of View
    | Error of (State * string)
    static member OfState(s: State) =
        match s.Error with
        | Some (err) -> Error(s, err.Message)
        | None -> View(View.OfState s)
    static member OfStateWithPrev(s: State) (prev: View) =
        match s.Error with
        | Some (err) -> Error(s, err.Message)
        | None -> View(View.OfStateWithPrev s prev)

let renderViewModel (previous: ViewOrError option) (s: State) = 
  let prev = 
    match previous with
    | None -> None
    | Some(prev) -> 
      match prev with
      | Error (_) -> None
      | View (v) -> Some(v)
  match prev with
  | None -> ViewOrError.OfState s
  | Some(v) -> ViewOrError.OfStateWithPrev s v
  
  

let executeStateUpdate action historyRecord oldState =
  let s = 
    match historyRecord with
    | Some(recrd) -> { oldState with InteractionHistory = recrd :: oldState.InteractionHistory }
    | None -> oldState
  Engine.execute action s

let executeDialogStateUpdate state text actor action actionText =
  executeStateUpdate 
    action
    (Some({Text = text; Actor = actor; UserReply = actionText }))
    state
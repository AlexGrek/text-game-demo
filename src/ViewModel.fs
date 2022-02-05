module ViewModel

open RichText
open LocationHub
open PersonHub
open State
open Actions

type DialogActorView =
    | UnknownActor of string
    | NoActor
    with
      member x.asString() =
        match x with 
        | UnknownActor(a) -> Some(a)
        | NoActor -> None
// todo: add known actor with link to person

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

let makeActor = function
  | None -> NoActor
  | Some(actorId) -> UnknownActor(actorId)

type DialogViewModel =
    { Actor: DialogActorView
      Text: RichText
      Variants: DialogVariantView list }
    static member OfDialogWindow (s: State) (dwindow: Dialog.DialogWindow) =
      match dwindow with
      | Dialog.TextWindow(d) ->
        { Actor = makeActor(d.Actor)
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
        DisplayName = d.Name //TODO: change this to actual display name
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

type PersonHubViewModel =
    { Text: RichText
      DisplayName: string
      Design: HubDesign.HubDesign
      Variants: DialogVariantView list }
    static member OfPersonHub (s: State) (d: PersonHub) =
      {
        Text = d.Description s
        DisplayName = d.Name //TODO: change this to actual display name
        Design = d.Design
        Variants =
          ((List.map (DialogVariantView.OfDialogVariant s) (d.Variants s))
          @ (List.singleton <| DialogVariantView.MakeUnlockedVariant "спросить о..." (DSL.pushWindow d.FactsDialogLink)))
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

type View =
    { Facts: Facts.Fact list
      UI: UIView }
    static member OfState(s: State) =
        { UI = UIView.OfUiState s
          Facts = renderFacts s.KnownFacts }


type ViewOrError =
    | View of View
    | Error of (State * string)
    static member OfState(s: State) =
        match s.Error with
        | Some (err) -> Error(s, err.Message)
        | None -> View(View.OfState s)

let renderViewModel (s: State) = ViewOrError.OfState s

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
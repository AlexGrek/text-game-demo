module ViewModel

open RichText
open LocationHub
open State
open Actions

type DialogActorView =
    | UnknownActor of string
    | NoActor
// todo: add known actor with link to person

type DialogVariantView =
    { Text: string
      Action: IAction
      IsLocked: Dialog.DialogOptionLock }
    static member OfDialogVariant (s: State) (d: Dialog.DialogVariant) =
        { Text = d.Text
          Action = d.Action
          IsLocked = d.IsLocked s }

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

type DialogViewModel =
    { Actor: DialogActorView
      Text: RichText
      Variants: DialogVariantView list }
    static member OfDialogWindow (s: State) (d: Dialog.DialogWindow) =
        { Actor = UnknownActor(d.Actor)
          Text = d.Text s
          Variants =
            List.map (DialogVariantView.OfDialogVariant s) (d.Variants s)
            |> filterOutVariants id }

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

type UIView =
    | DialogView of DialogViewModel
    | LocaitionHubView of LocationHubViewModel
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

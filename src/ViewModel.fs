module ViewModel

open RichText
open LocationHub
open State
open Actions

type DialogActorView = UnknownActor of string
// todo: add known actor with link to person

type DialogVariantView =
    { Text: string
      Action: IAction
      IsLocked: Dialog.DialogOptionLock }
    static member OfDialogVariant (s: State) (d: Dialog.DialogVariant) =
        { Text = d.Text
          Action = d.Action
          IsLocked = d.IsLocked s }

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

type LocaitionHubViewModel =
    { Text: RichText
      DisplayName: string
      Variants: Dialog.DialogVariant
      Design: HubDesign.HubDesign
      Locations: LocationHubVariant list
      Persons: LocationHubVariant list }

type UIView =
    | DialogView of DialogViewModel
    | LocaitionHubView of LocaitionHubViewModel
    static member OfUiState(s: State) =
        match s.UI with
        | DialogMode (dstate) ->
            let ref = dstate.Reference
            let dialog = Data.getGlobal Dialog.REPO_DIALOG ref.D
            let window = dialog.DialogWindows.[ref.W]
            DialogView(DialogViewModel.OfDialogWindow s window)
        | _ -> failwith "not implemented yet, punch developer in his fase for this"

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

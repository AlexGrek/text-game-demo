module State

open Data

type DialogState = { Reference: UReference }
type LocationHubState = { LocReference: string }

type UiState = 
    | DialogMode of DialogState
    | LocationHubMode of LocationHubState

type IGameData =
    abstract Stringify : unit -> string

type State =
    { UI: UiState
      UIStack: UiState list
      Previous: State option
      Log: string list
      KnownFacts: Set<string>
      Iteration: int
      Data: Map<string, obj> }

let makeInitialStateInDialog (r: UReference) =
    { UI = DialogMode({ Reference = r })
      UIStack = []
      Previous = None
      Log = []
      KnownFacts = Set.empty
      Iteration = 0
      Data = Map.empty }

let currentDialogRef s =
    match s.UI with
    | DialogMode (ds) -> ds.Reference.D
    | s -> failwith <| sprintf "cannot get current dialog ref while in %A" s

let private changeUIDialogWindow refs =
    function
    | DialogMode (ds) -> DialogMode({ ds with Reference = { ds.Reference with W = refs } })
    | LocationHubMode (_) -> failwith "cannot move to dialog window while in location hub"

let private setUIDialog d w =
    DialogMode({ Reference = { W = w; D = d } })

let private setUIDialogRef dw =
    DialogMode({ Reference = dw })

let private iterate s = { s with Iteration = s.Iteration + 1}

let gotoDialogWindow refs s =
    { iterate s with UI = changeUIDialogWindow refs s.UI }

let toDialog d w state = { iterate state with UI = setUIDialog d w }

let pushDialog d w s =
    { iterate s with
        UI = setUIDialog d w
        UIStack = s.UI :: s.UIStack }

let popDialog s =
    match s.UIStack with
    | [] -> failwith "Attempt to pop UI stack when it was empty"
    | head :: tail -> { iterate s with UI = head; UIStack = tail }

let jumpWithDialogStackTo targetRef newStackRefs s =
    let mapUIStack (u: UReference) =
        DialogMode({ Reference = u })
    let newStack = List.map mapUIStack newStackRefs
    { iterate s with UI = (setUIDialogRef targetRef); UIStack = newStack }
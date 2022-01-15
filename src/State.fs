module State

open Data
open System.Runtime

type DialogState = { Reference: UReference }

type UiState = DialogMode of DialogState

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
// | _ -> ""

let private changeUIDialogWindow refs =
    function
    | DialogMode (ds) -> DialogMode({ ds with Reference = { ds.Reference with W = refs } })

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
module State

open Data

type DialogState = { Reference: UReference }
type LocationHubState = { LocReference: string }
type PersonHubState = { PersonHubReference: string }

type UiState = 
    | DialogMode of DialogState
    | LocationHubMode of LocationHubState
    | PersonHubMode of PersonHubState

type InteractionHistoryRecord =
    {
        Actor: string option
        Text: string
        UserReply: string
    }

type ErrorInfo = {
    Message: string
}
type State =
    { UI: UiState
      UIStack: UiState list
      Previous: State option
      InteractionHistory: InteractionHistoryRecord list
      Log: string list
      KnownFacts: Set<string>
      KnownPersons: Map<string, Set<string>>
      Iteration: int
      Data: Map<string, obj>
      Error: ErrorInfo option }

let makeInitialStateInDialog (r: UReference) =
    { UI = DialogMode({ Reference = r })
      UIStack = []
      Previous = None
      InteractionHistory = []
      Log = []
      KnownFacts = Set.empty
      KnownPersons = Map.empty
      Iteration = 0
      Error = None
      Data = Map.empty }

let currentDialogRef s =
    match s.UI with
    | DialogMode (ds) -> ds.Reference.D
    | s -> failwith <| sprintf "cannot get current dialog ref while in %A" s

let private changeUIDialogWindow refs =
    function
    | DialogMode (ds) -> DialogMode({ ds with Reference = { ds.Reference with W = refs } })
    | LocationHubMode (_) -> failwith "cannot move to dialog window while in location hub"
    | PersonHubMode (_) -> failwith "cannot move to dialog window while in person hub"

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

let pushLocation name s =
    { iterate s with
        UI = LocationHubMode({LocReference = name})
        UIStack = s.UI :: s.UIStack }

let pushPerson name s =
    { iterate s with
        UI = PersonHubMode({PersonHubReference = name})
        UIStack = s.UI :: s.UIStack }

let popDialog s =
    match s.UIStack with
    | [] -> failwith "Attempt to pop UI stack when it was empty (popDialog)"
    | head :: tail -> { iterate s with UI = head; UIStack = tail }

let popToLocation sold =
    let rec popInner s =
        match s.UIStack with
        | [] -> failwith "Attempt to pop UI stack when it was empty (popToLocation)"
        | head :: tail -> 
            let pop1 = { s with UI = head; UIStack = tail }
            match pop1.UI with 
            | LocationHubMode(_) -> pop1
            | _ -> popInner pop1 // pop recursively until we hit a location
    iterate sold
    |> popInner

let changeLocation name s =
    match s.UI with
    | LocationHubMode(_) -> {iterate s with UI = LocationHubMode({LocReference = name})}
    | _ -> 
        // we have to push stack until we hit location, and then replace it
        { popToLocation s with UI = LocationHubMode({LocReference = name})}

let jumpWithDialogStackTo targetRef newStackRefs s =
    let mapUIStack (u: UReference) =
        DialogMode({ Reference = u })
    let newStack = List.map mapUIStack newStackRefs
    { iterate s with UI = (setUIDialogRef targetRef); UIStack = newStack }

let gameVersion = "0.1.3 alpha"
module Engine

open State
open Data
open Dialog
open LocationHub

let lookupCurrentDialogWindow s =
    let getFromUi = function
    | DialogMode d -> (d.Reference)
    | s -> failwith (sprintf "Cannot get current dialog window while in %A" s)
    let reference = getFromUi s.UI
    let dialog = Data.getGlobal<Dialog> REPO_DIALOG reference.D
    if (Map.containsKey reference.W >> not) dialog.DialogWindows then
        failwith <| sprintf "Reference error: Window <%s> not found in Dialog <%s>, found: %A"
                            reference.W
                            reference.D
                            (Map.toList dialog.DialogWindows |> List.map fst)
    dialog.DialogWindows.[reference.W]

let lookupCurrentLocation s =
    let getFromUi = function
    | LocationHubMode l -> (l.LocReference)
    | s -> failwith (sprintf "Cannot get current location window while in %A" s)
    let reference = getFromUi s.UI
    getGlobal<LocationHub.LocationHub> REPO_LOCATIONS reference
    

let executeCurrentDialogWindow s =
    let dialogWindow = lookupCurrentDialogWindow s
    match dialogWindow.OnEntry with
    | None -> s
    | Some(m) -> m s

let execute (a: Actions.IAction) (s: State) =
    try 
        let updatedState = a.Exec s
        match updatedState.UI with
        | DialogMode(d) -> executeCurrentDialogWindow updatedState
        | _ -> updatedState // other modes do not require execution for now
    with
        | Failure(f) -> {s with Error = Some({Message = f})}
    

type IGameRunner =
    abstract member InitialState : unit -> State
    abstract member Build : unit -> unit

let mutable private _gameToLoadOption: Option<IGameRunner> = None

let setGameRunner game =
    match _gameToLoadOption with
    | None -> 
        _gameToLoadOption <- Some(game)
        printfn "loading game..."
        game.Build()
    | Some(_) -> failwith ("Trying to load game when game was already loaded: " + game.ToString())

let getGameRunner() =
    match _gameToLoadOption with
    | None -> failwith "no game to load"
    | Some(g) -> g

let reset () = 
    resetGlobal<Dialog> REPO_DIALOG
    resetAll ()
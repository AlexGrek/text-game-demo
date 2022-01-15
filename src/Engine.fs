module Engine

open State
open Data
open Dialog

let lookupCurrentDialogWindow s =
    let getFromUi = function
    | DialogMode d -> (d.Reference)
    let reference = getFromUi s.UI
    let dialog = Data.getGlobal<Dialog> REPO_DIALOG reference.D
    if (Map.containsKey reference.W >> not) dialog.DialogWindows then
        failwith <| sprintf "Reference error: Window <%s> not found in Dialog <%s>, found: %A"
                            reference.W
                            reference.D
                            (Map.toList dialog.DialogWindows |> List.map fst)
    dialog.DialogWindows.[reference.W]

let executeCurrentDialogWindow s =
    let dialogWindow = lookupCurrentDialogWindow s
    match dialogWindow.OnEntry with
    | None -> s
    | Some(m) -> m s

let execute (a: Actions.IAction) =
    a.Exec >> executeCurrentDialogWindow
    

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
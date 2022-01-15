module DevTools


open Feliz
open Feliz.Router

open Dialog
open RichText
open Game
open State
open Fable.SimpleJson

type Components() =
    [<ReactComponent>]
    static member DevToolsToolbar(enabled: bool, state: State, setGameState) =
        if enabled then
            Html.div [ prop.className "dev-toolbar-footer"
                       prop.children [ Html.button [ prop.text "Dump json state to console"
                                                     prop.onClick (fun _ ->
                                                         Json.serialize<State> state |> printfn "%A") ]
                                       Html.button [ prop.text "Save state"
                                                     prop.onClick (fun _ -> Serialication.saveMainState state) ]
                                       Html.button [ prop.text "Load state"
                                                     prop.onClick (fun _ ->
                                                         printfn "state loaded"
                                                         setGameState <| Serialication.loadMainState ()) ] ] ]
        else
            Html.div []
module DevTools


open Feliz
open Feliz.Router
open State
open Fable.SimpleJson

type p = prop

type Components() =
    [<ReactComponent>]
    static member DevToolsToolbar(enabled: bool, state: State, setGameState) =
        if enabled then
            Html.div [ prop.className "dev-toolbar-footer"
                       prop.children [ Html.button [ prop.text "Dump json state to console"
                                                     prop.onClick (fun _ ->
                                                         Json.serialize<State> state |> printfn "%A") ]
                                       Html.button [ prop.text "Save state"
                                                     prop.onClick (fun _ -> Serialization.saveMainState state) ]
                                       Html.button [ prop.text "Load state"
                                                     prop.onClick (fun _ ->
                                                         printfn "state loaded"
                                                         setGameState <| Serialization.loadMainState ()) ]
                                       Html.span [prop.text ("Version: " + gameVersion ); prop.className "version-span" ] ] ]
        else
            Html.div []

    [<ReactComponent>]
    static member ErrorPage(errorText: string, state: State, setGameState: State -> unit) =
            Html.div [
                p.children [
                    Html.h1 [
                        prop.innerHtml "Error happened!"
                    ]
                    Html.p [
                        prop.innerHtml errorText
                    ]
                    Html.button [
                        prop.text "[ Recover ]"
                        prop.onClick (fun _ -> setGameState {state with Error = None})
                    ]
                    Html.h3 [
                        prop.innerHtml "Error state:"
                    ]
                    Html.textarea [
                        prop.value (Json.serialize<State> {state with InteractionHistory = []})
                        prop.style [ style.minHeight 100; style.minWidth 600 ]
                    ]
                    
                ]
            ]
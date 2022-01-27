module DebuggerRenderer
open Feliz

type DebuggerPanelRenderer() =
    [<ReactComponent>]
    static member DebuggerPanel(state: State.State) =
        Html.div [ prop.className "debug-panel-outer"
                   prop.children [

                                   Html.div [ prop.className "debug-panel-container"
                                              prop.children (
                                                  List.map (fun (k, v) -> DebuggerPanelRenderer.DataFieldRenderer(k, v)) (Map.toList state.Data)
                                              ) ] ]
                    ]

    static member DataFieldRenderer(key, value) =
        Html.div [ prop.className "debug-data-record"
                   prop.children [ 
                    Html.p [
                        prop.className "debug-key"
                        prop.text key
                    ]
                    Html.p [
                        prop.className "debug-val"
                        prop.text (sprintf "%A" value)
                    ] ] ]

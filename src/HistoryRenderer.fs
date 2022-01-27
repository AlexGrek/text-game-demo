module HistoryRenderer

open Feliz

type HistoryPanelRenderer() =
    [<ReactComponent>]
    static member HistoryPanel(records: State.InteractionHistoryRecord array) =
        Html.div [ prop.className "history-panel-outer"
                   prop.children [

                                   Html.div [ prop.className "history-panel-container"
                                              prop.children (
                                                  Array.map (fun s -> HistoryPanelRenderer.HistoryRenderer(s)) records
                                              ) ] ]
                    ]

    [<ReactComponent>]
    static member HistoryRenderer(record: State.InteractionHistoryRecord) =
        let actor = 
            match record.Actor with
            | Some(act) -> 
                Html.p [ 
                        prop.className "history-actor"
                        prop.text act ]
            | None -> Html.div []


        Html.div [ prop.className "history-record"
                   prop.children [ Html.div [ prop.className "history-record-icon" ]
                                   Html.div [ prop.className "history-record-container"
                                              prop.children [ actor
                                                              Html.p [ prop.className "history-record-text"
                                                                       prop.text record.Text ]
                                                              Html.p [ prop.className "history-record-reply"
                                                                       prop.text record.UserReply ] ] ]

                                    ] ]

module FactsRenderer

open Feliz

type FactsPanelRenderer() =
    [<ReactComponent>]
    static member FactsPanel(facts: string list) =
        Html.div [ prop.className "facts-panel-outer"
                   prop.children [

                                   Html.div [ prop.className "facts-panel-container"
                                              prop.children (
                                                  List.map (fun s -> FactsPanelRenderer.FactRenderer(s)) facts
                                              ) ] ]

                    ]

    [<ReactComponent>]
    static member FactRenderer(fid: string) =
        let fact = Facts.lookupFact fid

        Html.div [ prop.className "fact"
                   prop.children [ Html.div [ prop.className "fact-icon" ]
                                   Html.div [ prop.className "fact-text-container"
                                              prop.children [ Html.p [ prop.className "fact-id"
                                                                       prop.text fact.Name ]
                                                              Html.p [ prop.className "fact-desc"
                                                                       prop.text fact.Description ] ] ]

                                    ] ]

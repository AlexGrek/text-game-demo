module UiUtils

open Feliz

let addStyleBasedOn cond baseStyle addition =
    if cond then
        baseStyle + " " + addition
    else
        baseStyle

let chooseStyleBasedOn cond baseStyle onTrue onFalse =
    if cond then
        baseStyle + " " + onTrue
    else
        baseStyle + " " + onFalse

type PanelHeaderModel = 
    {
        header: string;
        onClose: unit -> unit
    }

type PanelButtonModel =
    {
        text: string; icon: string; key: int; onClick: unit -> unit
    }

type PanelUtils() =
    [<ReactComponent>]
    static member PanelHeader(model) =
        Html.div [
            prop.className "panel-header"
            prop.children [
                Html.p [
                    prop.text model.header
                ]
                Html.button [
                    prop.className "panel-close-button"
                    prop.onClick (fun _ -> model.onClose())
                    prop.children [
                        Html.img [
                            prop.alt "close"
                            prop.src "img/free-icon-cross.png"
                        ]
                    ]
                ]
            ]
        ]

    [<ReactComponent>]
    static member PanelButton(model) =
        Html.button [
            prop.className "panel-button animate__animated animate__heartBeat"
            prop.onClick (fun _ -> model.onClick())
            prop.key model.key
            prop.children [
                Html.img [
                    prop.className "panel-button-icon"
                    prop.src model.icon
                ]
                Html.span [
                    prop.className "panel-button-span"
                    prop.innerHtml model.text
                ]
            ]
        ]
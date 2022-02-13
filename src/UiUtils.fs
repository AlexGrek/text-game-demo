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

let lookupIcon iconName asDefault =
        match iconName with
        | "dialog-exit-icon" -> "img/comment-check.png"
        | "dialog-default-icon" -> "img/omment-info.png"
        | _ -> asDefault

type PanelUtils() =
    [<ReactComponent>]
    static member PanelHeader(header: string, onClose) =
        Html.div [
            prop.className "panel-header"
            prop.children [
                Html.p [
                    prop.text header
                ]
                Html.button [
                    prop.className "panel-close-button"
                    prop.onClick (fun _ -> onClose())
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
    static member PanelButton(text: string, icon: string, key: int, onClick: unit -> unit) =
        Html.button [
            prop.className "panel-button animate__animated animate__heartBeat"
            prop.onClick (fun _ -> onClick())
            prop.key key
            prop.children [
                Html.img [
                    prop.className "panel-button-icon"
                    prop.src icon
                ]
                Html.span [
                    prop.className "panel-button-span"
                    prop.innerHtml text
                ]
            ]
        ]
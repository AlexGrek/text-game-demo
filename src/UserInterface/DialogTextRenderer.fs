namespace App

open Feliz
open ViewModel
open RichText

type DialogTextComponents() =
    static let getIconForAuthor author =
        "img/police-badge.png"

    [<ReactComponent>]
    static member DialogtextRenderer(animation, text: RichText, s: State.State, iteration: int) =
        Html.div [  prop.className "dialog-text"
                    prop.key iteration
                    prop.children [ (RichTextComponents.RichTextRenderer(
                                            text,
                                            animation
                                )) ] ]
        
    
    [<ReactComponent>]
    static member AuthorRenderer(author: DialogActorView) =
        match author with
        | UnknownActor(name) ->
            Html.div [
                prop.className "author-container"
                prop.children [
                    Html.div [
                        prop.className "inner-author-container"
                        prop.children [
                            Html.div [
                                prop.className "author-icon"
                                prop.children [
                                    Html.img [
                                        prop.alt "author icon"
                                        prop.src (getIconForAuthor author)
                                    ]
                                ]
                            ]
                            Html.div [
                                prop.className "author-name"
                                prop.innerHtml name
                            ]
                        ]
                    ]
                ]
            ]
        | NoActor ->
            Html.div [
                prop.className "invisible"
            ]


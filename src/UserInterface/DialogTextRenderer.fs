namespace App

open Feliz

type DialogTextComponents() =
    static let getIconForAuthor author =
        "img/police-badge.png"

    [<ReactComponent>]
    static member DialogtextRenderer(animation, textG, s: State.State, iteration: int) =
        Html.div [  prop.className "dialog-text"
                    prop.key iteration
                    prop.children [ (RichTextComponents.RichTextRenderer(
                                            textG s,
                                            animation
                                )) ] ]
        
    
    [<ReactComponent>]
    static member AuthorRenderer(author: string) =
        Html.div [
            prop.className (if author = "" then "invisible" else "author-container")
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
                            prop.innerHtml author
                        ]
                    ]
                ]
            ]
        ]


namespace App

open Feliz
open ViewModel
open RichText
open State

type DialogTextComponents() =
    static let getIconForAuthor author =
        match author with
        | "joe" -> "img/police-badge.png"
        | "babka" -> "img/wheelchair.png"
        | _ -> "img/rhombus.png"

    [<ReactComponent>]
    static member DialogtextRenderer(animation, gameAnimation: UIAnimation, text: RichText, s: State.State, iteration: int) =
        let UIAnimationClass = 
            match gameAnimation with
            | Warning -> "animate__shakeX animate__animated"
            | _ -> ""
        Html.div [ prop.className ("dialog-text " + UIAnimationClass)
                   prop.key iteration
                   prop.children [ (RichTextComponents.RichTextRenderer(text, animation)) ] ]

    [<ReactComponent>]
    static member AuthorPresentRenderer(actor, id) =
        Html.div [ prop.className "author-container"
                   prop.children [ Html.div [ prop.className
                                                  "inner-author-container animate__bounceInLeft animate__faster animate__animated"
                                              prop.children [ Html.div [ prop.className "author-icon"
                                                                         prop.children [ Html.img [ prop.alt
                                                                                                        "author icon"
                                                                                                    prop.src (
                                                                                                        getIconForAuthor
                                                                                                            id
                                                                                                    ) ] ] ]
                                                              Html.div [ prop.className "author-name"
                                                                         prop.innerHtml actor ] ] ] ] ]


    [<ReactComponent>]
    static member AuthorRenderer(author: DialogActorView) =
        match author with
        | UnknownActor (name) -> DialogTextComponents.AuthorPresentRenderer(name, name)
        | NoActor -> Html.div [ prop.className "invisible" ]
        | RealActor (pers, text) -> DialogTextComponents.AuthorPresentRenderer(text, pers.Name)

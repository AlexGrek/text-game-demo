namespace App

open Feliz
open RichText

type RichTextComponents() =
    [<ReactComponent>]
    static member RichTextElementRenderer(el: RichTextElement) =
        match el with
        | Text(s) -> Html.span s
        | HighlightedAsValue(s) -> Html.span [
                                    prop.className "highlighted-as-value"
                                    prop.text s
                                ]
        | Bold(s) -> Html.span [
                        prop.className "bold-text"
                        prop.text s
                    ]

    [<ReactComponent>]
    static member RichTextRenderer(el: RichTextElement list, animationClasses: string) =
        Html.p [
            prop.className animationClasses
            prop.children (List.map RichTextComponents.RichTextElementRenderer el)
        ]
        


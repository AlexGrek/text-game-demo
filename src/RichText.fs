module RichText

open System.Text.RegularExpressions

type RichTextElement = 
    | Text of string
    | Bold of string
    | HighlightedAsValue of string

type RichText = RichTextElement list

let removeBraces (sWithBraces: string) =
    sWithBraces.Substring(1, sWithBraces.Length - 2) // remove braces

// replace *(var)* with variable values
// replace *! !* with highlighted text
let parse (txt: string) (state: State.State) =
    let replaceVar (sWithBraces: string) =
        let s = removeBraces sWithBraces
        if (Map.containsKey s state.Data) then
            HighlightedAsValue((state.Data.Item s).ToString())
        else
            Bold(s)
    let isVar (s: string) =
        s.StartsWith "(" && s.EndsWith ")"
    let isHighlighted (s: string) =
        s.StartsWith "!" && s.EndsWith "!" && s.Length > 2
    let mapper (el: string) =
        if (isVar el) then
            replaceVar el
        else
            if (isHighlighted el) then
                Bold(removeBraces el)
            else
                Text(el)
    
    let uniteTextNodes acc el =
        match el with 
        | Text(s) -> 
            match acc with 
            | Text(t) :: tail -> Text(t + s) :: tail
            | _ -> el :: acc
        | _ -> el :: acc

    txt.Split("*")
    |> Seq.map mapper
    |> Seq.fold uniteTextNodes []
    |> List.rev

let text txt = 
    [ Text(txt) ]

let bold txt =
    Bold(txt)

let toString rt =
    List.fold (fun (acc: string) (el: RichTextElement) -> 
        match el with
        | Text(t) -> acc + t
        | HighlightedAsValue(t) -> acc + t
        | Bold(t) -> acc + "*" + t + "*" ) "" rt
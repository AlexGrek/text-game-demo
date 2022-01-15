module StoryWriter

// high-level functions to create

open Dialog
open State

let nextIndex refit index = sprintf "%s%03i" refit index

let nextPage refit index =
    printfn "%A %A" refit index
    s [ makeUnlockedVariant "next" (nextIndex refit (index + 1) |> toWindow) ]

let nextTo target = s [ makeUnlockedVariant "next" (toWindow target) ]

let pageToNext rw index actor text =
    let refits = nextIndex rw index
    dialogWindow refits actor (s text) (nextPage rw index)

let pageTo refit index actor text target =
    let refits = nextIndex refit index
    dialogWindow refits actor (s text) (nextTo target)

let paginate refit actor texts target =
    let rec move index rest =
        match rest with
        | [ t ] ->
            List.singleton
            <| pageTo refit index actor t target
        | t :: tail ->
            pageToNext refit index actor t
            :: move (index + 1) tail
        | [] -> failwith "cannot paginate empty list"

    move 0 texts

let page name actor text variants =
    dialogWindow name actor text variants

// DIALOG COMPOSITION TOOLS

let addWindow dialog (window: DialogWindow) =
    { dialog with
              DialogWindows = Map.add window.Name window dialog.DialogWindows }

let addWindows dialog (windows: DialogWindow list) =
    {dialog with
            DialogWindows = Seq.fold (fun acc (el: DialogWindow) -> Map.add el.Name el acc) 
                                     dialog.DialogWindows 
                                     windows}

type DialogBuilder(name: string) =
    let addWindow dialog (window: DialogWindow) =
        { dialog with
              DialogWindows = Map.add window.Name window dialog.DialogWindows }

    member __.Yield(_) : Dialog =
        { Name = name
          DialogWindows = Map.empty
          DefaultWindow = None }

    member __.Run(dialog: Dialog) = 
        printfn "Dump: %A " <| Data.dumpGlobalKeys<Dialog> REPO_DIALOG
        Data.save<Dialog> REPO_DIALOG dialog.Name dialog

    [<CustomOperation("add")>]
    member __.Add(dialog, action: DialogWindow) : Dialog = addWindow dialog action

    [<CustomOperation("text")>]
    member __.Text(dialog, name: string, author: string, text: string, variants: DialogVariant list) =
        addWindow dialog <| page name author (stxt text) (s variants)

let dialog x = DialogBuilder(x)


type StaticWindowGenerator = { 
    Name: string
    Actor: string
    Text: State -> RichText.RichText
    StaticVariants: DialogVariant list
    }
    with 
        member x.Build() =
            { 
                Name = x.Name
                Actor = x.Actor
                Text = x.Text
                Variants = s x.StaticVariants
                OnEntry = None
            }

type WindowBuilder(name: string) =
    let staticDialogWindow name actor textGen variants =
        { Name = name; Actor = actor; Text = textGen; StaticVariants = variants }

    member __.Yield(_) : StaticWindowGenerator = 
        staticDialogWindow name "" (stxt "") []

    member __.Run(dialog: StaticWindowGenerator): DialogWindow = 
        dialog.Build()

    [<CustomOperation("stxt")>]
    member __.Get(dialog: StaticWindowGenerator, action: string) : StaticWindowGenerator =
        staticDialogWindow name dialog.Actor (stxt "") dialog.StaticVariants

    [<CustomOperation("var")>]
    member __.Get2(dialog: StaticWindowGenerator, variant: DialogVariant) : StaticWindowGenerator =
        staticDialogWindow
            name
            dialog.Actor
            dialog.Text
            ( variant :: dialog.StaticVariants )

let windowgen x = WindowBuilder(x)


type VariantBuilder(name: string) =

    member __.Yield(_) : DialogVariant =  {
                                            Text = ""; 
                                            Action = {Actions.ToWindow.Target = "hui"};
                                            Locked = AlwaysUnlocked
                                            }

    member __.Run(v: DialogVariant) = v

    [<CustomOperation("hello")>]
    member __.Get(v: DialogVariant, text: string) : DialogVariant =
        makeUnlockedVariant text v.Action

let vargen name = VariantBuilder(name)
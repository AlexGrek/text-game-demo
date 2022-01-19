namespace App

open Feliz
open Feliz.Router

open Dialog
open RichText
open Game
open State
open Fable.SimpleJson
open Fable.Core
open Fable.Core.JS
open FactsRenderer
open LocationHub

type AnimationProgress =
    | NoAnimation
    | VariantChosen of int


type GlobalState =
    { DevTools: bool
      FactsPanelOpened: bool
      GameState: State
      Animation: AnimationProgress }

type Components() =
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() =
        let initialState = Engine.getGameRunner().InitialState()

        let (state, setState) =
            React.useState (
                { GameState = initialState
                  Animation = NoAnimation
                  FactsPanelOpened = false
                  DevTools = true }
            )

        let setGameState s =
            setState
                { state with
                    GameState = s
                    Animation = NoAnimation }

        
        match state.GameState.Error with
        | None ->
            try
                let uiWidgetToRender =
                    match state.GameState.UI with
                    | DialogMode (dm) -> 
                        Components.DialogWindowView(
                                               (Engine.lookupCurrentDialogWindow state.GameState),
                                               state,
                                               state.Animation,
                                               setState,
                                               setGameState
                                           )
                    | LocationHubMode (hub) ->
                        Components.LocationHubView(
                            (Engine.lookupCurrentLocation state.GameState),
                            state,
                            state.Animation,
                            setState,
                            setGameState
                        )

                Html.div [ prop.className "main-div"
                           prop.children [ Components.PopupPanel(state, setState)
                                           Components.HeaderPanel(state, setState)
                                           uiWidgetToRender
                                           (DevTools.Components.DevToolsToolbar(state.DevTools, state.GameState, setGameState)) ] ]
                with
                | Failure(msg) ->
                    DevTools.Components.ErrorPage(msg, state.GameState, setGameState)
            | Some(err) ->
                DevTools.Components.ErrorPage(err.Message, state.GameState, setGameState)


    [<ReactComponent>]
    static member HeaderPanel(s: GlobalState, ss: GlobalState -> unit) =
        let factsButton =
            Html.button [ prop.className "header-panel-button"
                          prop.onClick (fun _ -> ss { s with FactsPanelOpened = true })

                           ]

        Html.div [ prop.className "header-panel"
                   prop.children [ UiUtils.PanelUtils.PanelButton(
                                       text = "Факты",
                                       icon = "img/free-icon-bulb.png",
                                       key = s.GameState.KnownFacts.Count,
                                       onClick = (fun _ -> ss { s with FactsPanelOpened = true })
                                   ) ] ]

    [<ReactComponent>]
    static member PopupPanel(s: GlobalState, ss: GlobalState -> unit) =
        let factsPanelStyle =
            match (not s.FactsPanelOpened) with
            | true -> "animate__animated animate__fadeOutUp animate__faster"
            | false -> "animate__animated animate__fadeInDown animate__faster"

        Html.div [ prop.className ("popup-panel " + factsPanelStyle)
                   prop.children [ UiUtils.PanelUtils.PanelHeader(
                                       "Факты",
                                       fun _ -> ss { s with FactsPanelOpened = false }
                                   )
                                   FactsPanelRenderer.FactsPanel(Seq.toList s.GameState.KnownFacts) ] ]


    [<ReactComponent>]
    static member LocationHubView 
        ( 
            loc: LocationHub,
            s: GlobalState,
            a: AnimationProgress,
            setstate: GlobalState -> unit,
            setgs 
        ) =
        let animation =
            match s.Animation with
            | NoAnimation -> "animate__animated animate__fadeInLeft animate__faster"
            | VariantChosen (_) -> "animate__animated animate__fadeOutRight"

        let renderVariant (el: DialogVariant) =
            match (el.IsLocked s.GameState) with
            | Unlocked -> Some(Components.DialogButton, el)
            | Reason (_) -> Some(Components.LockedDialogButton, el)
            | Hidden -> None

        let toSimpleVariants (l: LocationHub.LocationHubVariant list) =
            List.map (fun r -> r.Variant) l

        let renderedVariants = 
            List.choose renderVariant (loc.Variants s.GameState) 
                            |> List.mapi (fun i (render, d) ->
                                                      render (d, s, a, setstate, setgs, i))

        let renderedLocations = 
            List.choose renderVariant (toSimpleVariants loc.Locations) 
                            |> List.mapi (fun i (render, d) ->
                                                      render (d, s, a, setstate, setgs, i))

        let renderedPersons = 
            List.choose renderVariant (loc.Persons s.GameState |> toSimpleVariants) 
                            |> List.mapi (fun i (render, d) ->
                                                      render (d, s, a, setstate, setgs, i))

        Html.div [
            prop.className "location-hub-window dialog-window"
            prop.children [
                DialogTextComponents.DialogtextRenderer(
                                       animation,
                                       loc.Description,
                                       s.GameState,
                                       s.GameState.Iteration
                                   )
                Html.div [ 
                    prop.className "variants"
                    prop.children (
                            renderedVariants
                            @ [
                                Html.div [ prop.innerHtml "people to talk" ]
                            ]
                            @ renderedPersons
                            @ [
                                Html.div [ prop.innerHtml "places to go" ]
                            ]
                            @ renderedLocations
                        )
                ]
            ]
        ]
        

    [<ReactComponent>]
    static member DialogWindowView
        (
            w: DialogWindow,
            s: GlobalState,
            a: AnimationProgress,
            setstate: GlobalState -> unit,
            setgs
        ) =
        let animation =
            match s.Animation with
            | NoAnimation -> "animate__animated animate__fadeInLeft animate__faster"
            | VariantChosen (_) -> "animate__animated animate__fadeOutRight"

        let render (el: DialogVariant) =
            match (el.IsLocked s.GameState) with
            | Unlocked -> Some(Components.DialogButton, el)
            | Reason (_) -> Some(Components.LockedDialogButton, el)
            | Hidden -> None

        Html.div [ prop.className "dialog-window"
                   prop.children [ DialogTextComponents.AuthorRenderer(w.Actor)
                                   DialogTextComponents.DialogtextRenderer(
                                       animation,
                                       w.Text,
                                       s.GameState,
                                       s.GameState.Iteration
                                   )
                                   Html.div [ prop.className "variants"
                                              prop.children (
                                                  List.choose render (w.Variants s.GameState)
                                                  |> List.mapi (fun i (render, d) ->
                                                      render (d, s, a, setstate, setgs, i))
                                              ) ]
                    ] 
        ]


    [<ReactComponent>]
    static member DialogButton(prp: Dialog.DialogVariant, (s: GlobalState), a, setter, setgs, i) =
        let withAnimation () =
            setTimeout
                (fun _ ->
                    setgs (Engine.execute prp.Action s.GameState)
                    |> ignore)
                500
            |> ignore // no way to stop timeout

            setter { s with Animation = VariantChosen(i) }

        let calcDelay i =
            if i = 0 then
                ""
            else
                sprintf "animate__delay-%ds" i

        let addClassesByAnimation =
            function
            | NoAnimation ->
                "animate__animated animate__backInUp animate__faster "
                + calcDelay i
            | VariantChosen (x) when x = i -> "animate__animated animate__zoomOutDown"
            | VariantChosen (_) -> "animate__animated animate__fadeOut"

        let addClassesByAction =
            match prp.Action with
            | :? Actions.OnlyOnce -> "onlyonce"
            | :? Actions.Pop -> "pop"
            | :? Actions.Push -> "push"
            | :? Actions.Conditional -> "conditional"
            | :? Actions.ToWindow -> "towindow"
            | :? Actions.ToWindowMod -> "towindiw towindowmod"
            | _ -> ""

        Html.button [ prop.text prp.Text
                      prop.key s.GameState.Iteration
                      prop.className (
                          addClassesByAction
                          + " variant "
                          + (addClassesByAnimation s.Animation)
                      )
                      prop.onClick (fun _ ->
                          match a with
                          | NoAnimation -> withAnimation ()
                          | _ -> () // do nothing on click when animation is playing
                      ) ]

    [<ReactComponent>]
    static member LockedDialogButton(prp: Dialog.DialogVariant, (s: GlobalState), a, setter, setgs, i) =
        let calcDelay i =
            if i = 0 then
                ""
            else
                sprintf "animate__delay-%ds" i

        let addClassesByAnimation =
            function
            | NoAnimation ->
                "animate__animated animate__backInUp animate__faster "
                + calcDelay i
            | VariantChosen (x) when x = i -> "animate__animated animate__zoomOutDown"
            | VariantChosen (_) -> "animate__animated animate__fadeOut"

        Html.button [ prop.text prp.Text
                      prop.key s.GameState.Iteration
                      prop.className (
                          "variant locked "
                          + (addClassesByAnimation s.Animation)
                      ) ]

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState (0)

        Html.div [ Html.h1 count
                   Html.button [ prop.onClick (fun _ -> setCount (count + 1))
                                 prop.text "Increment" ] ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState (Router.currentUrl ())

        React.router [ router.onUrlChanged updateUrl
                       router.children [ match currentUrl with
                                         | [] -> Html.h1 "Index"
                                         | [ "hello" ] -> Components.HelloWorld()
                                         | [ "counter" ] -> Components.Counter()
                                         | otherwise -> Html.h1 "Not found" ] ]

namespace App

open Feliz
open Feliz.Router

open Dialog
open RichText
open State
open Fable.Core.JS
open FactsRenderer
open ViewModel
open HistoryRenderer

type AnimationProgress =
    | NoAnimation
    | VariantChosen of int


type OpenedPanel =
    | NoPanel
    | HistoryPanel
    | FactsPanel
    | DebugPanel

type GlobalState =
    { DevTools: bool
      Panel: OpenedPanel
      GameState: State
      RenderedState: ViewOrError
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
                  RenderedState = renderViewModel initialState
                  Animation = NoAnimation
                  Panel = NoPanel
                  DevTools = true }
            )

        let setGameState s =
            setState
                { state with
                    GameState = s
                    Animation = NoAnimation
                    RenderedState = renderViewModel s }

        let vm = state.RenderedState

        match vm with
        | View (v) ->
            try
                let uiWidgetToRender =
                    match v.UI with
                    | DialogView (d) -> Components.DialogWindowView(d, state, state.Animation, setState, setGameState)
                    | LocaitionHubView (hub) ->
                        Components.LocationHubView(hub, state, state.Animation, setState, setGameState)
                    | PersonHubView (hub) ->
                        Components.PersonHubView(hub, state, state.Animation, setState, setGameState)

                Html.div [ prop.className "main-div"
                           prop.children [ Components.PopupPanel(state, setState)
                                           Components.HeaderPanel(state, setState)
                                           uiWidgetToRender
                                           (DevTools.Components.DevToolsToolbar(
                                               state.DevTools,
                                               state.GameState,
                                               setGameState
                                           )) ] ]
            with
            | Failure (msg) -> DevTools.Components.ErrorPage(msg, state.GameState, setGameState)
        | Error (state, err) -> DevTools.Components.ErrorPage(err, state, setGameState)


    [<ReactComponent>]
    static member HeaderPanel(s: GlobalState, ss: GlobalState -> unit) =
        Html.div [ prop.className "header-panel"
                   prop.children [ UiUtils.PanelUtils.PanelButton(
                                       text = "Факты",
                                       icon = "img/free-icon-bulb.png",
                                       key = s.GameState.KnownFacts.Count,
                                       onClick = (fun _ -> ss { s with Panel = FactsPanel })
                                   )
                                   UiUtils.PanelUtils.PanelButton(
                                       text = "История",
                                       icon = "img/free-icon-comment-alt.png",
                                       key = 999999999,
                                       onClick = (fun _ -> ss { s with Panel = HistoryPanel })
                                   )
                                   UiUtils.PanelUtils.PanelButton(
                                       text = "Дебаггер",
                                       icon = "img/debug.png",
                                       key = 999999990,
                                       onClick = (fun _ -> ss { s with Panel = DebugPanel })
                                   ) ] ]


    [<ReactComponent>]
    static member PopupPanel(s: GlobalState, ss: GlobalState -> unit) =
        let openClosedStyle property =
            match (not property) with
            | true -> "animate__animated animate__fadeOutUp animate__faster"
            | false -> "animate__animated animate__fadeInDown animate__faster"

        let anyPanelStyle = openClosedStyle (s.Panel <> NoPanel)

        let close = fun _ -> ss { s with Panel = NoPanel }

        let renderSpecificPanel name renderer =
            [ UiUtils.PanelUtils.PanelHeader(header = name, onClose = close)
              renderer ]

        let genChildren =
            match s.Panel with
            | NoPanel -> []
            | FactsPanel ->
                renderSpecificPanel "Факты"
                <| FactsPanelRenderer.FactsPanel(Seq.toList s.GameState.KnownFacts)
            | HistoryPanel ->
                renderSpecificPanel "История"
                <| HistoryPanelRenderer.HistoryPanel(List.toArray s.GameState.InteractionHistory)
            | DebugPanel ->
                renderSpecificPanel "Дебаггер"
                <| DebuggerRenderer.DebuggerPanelRenderer.DebuggerPanel(s.GameState)

        Html.div [ 
            prop.className ("popup-panel-holder " + anyPanelStyle)
            prop.children [
                Html.div [ prop.className "popup-panel"
                           prop.children 
                            genChildren ]
                Html.button [
                                    prop.className ("popup-panel-close-button")
                                    prop.text "закрыть"
                                    prop.onClick close
                                ] ] ]


    [<ReactComponent>]
    static member LocationHubView
        (
            loc: LocationHubViewModel,
            s: GlobalState,
            a: AnimationProgress,
            setstate: GlobalState -> unit,
            setUpdatedGameState
        ) =
        let animation =
            match s.Animation with
            | NoAnimation -> "animate__animated animate__fadeInLeft animate__faster"
            | VariantChosen (_) -> "animate__animated animate__fadeOutRight"

        let renderVariant (el: DialogVariantView) =
            match el.IsLocked with
            | Unlocked -> Some(Components.DialogButton, el)
            | Reason (_) -> Some(Components.LockedDialogButton, el)
            | Hidden -> None

        let toSimpleVariants (l: LocationHubVariantView list) = List.map (fun r -> r.Variant) l

        let setgs action actionText =
            setUpdatedGameState
            <| executeDialogStateUpdate
                s.GameState
                (toString loc.Text)
                (Some(loc.DisplayName)) // display location name as actor
                action
                actionText

        let renderedVariants =
            List.choose renderVariant loc.Variants
            |> List.mapi (fun i (render, d) -> render (d, s, a, setstate, setgs, i))

        let renderedLocations =
            List.choose renderVariant (toSimpleVariants loc.Locations)
            |> List.mapi (fun i (render, d) -> render (d, s, a, setstate, setgs, i))

        let renderedPersons =
            List.choose renderVariant (loc.Persons |> toSimpleVariants)
            |> List.mapi (fun i (render, d) -> render (d, s, a, setstate, setgs, i))

        Html.div [ prop.className "location-hub-window dialog-window"
                   prop.children [ DialogTextComponents.DialogtextRenderer(
                                       animation,
                                       loc.Text,
                                       s.GameState,
                                       s.GameState.Iteration
                                   )
                                   Html.div [ prop.className "variants"
                                              prop.children (
                                                  renderedVariants
                                                  @ [ Html.div [ prop.innerHtml "people to talk" ] ]
                                                    @ renderedPersons
                                                      @ [ Html.div [ prop.innerHtml "places to go" ] ]
                                                        @ renderedLocations
                                              ) ] ] ]


    [<ReactComponent>]
    static member PersonHubView
        (
            loc: PersonHubViewModel,
            s: GlobalState,
            a: AnimationProgress,
            setstate: GlobalState -> unit,
            setUpdatedGameState
        ) =
        let animation =
            match s.Animation with
            | NoAnimation -> "animate__animated animate__fadeInLeft animate__faster"
            | VariantChosen (_) -> "animate__animated animate__fadeOutRight"

        let renderVariant (el: DialogVariantView) =
            match el.IsLocked with
            | Unlocked -> Some(Components.DialogButton, el)
            | Reason (_) -> Some(Components.LockedDialogButton, el)
            | Hidden -> None

        let toSimpleVariants (l: LocationHubVariantView list) = List.map (fun r -> r.Variant) l

        let setgs action actionText =
            setUpdatedGameState
            <| executeDialogStateUpdate
                s.GameState
                (toString loc.Text)
                (Some(loc.DisplayName)) // display location name as actor
                action
                actionText

        let renderedVariants =
            List.choose renderVariant loc.Variants
            |> List.mapi (fun i (render, d) -> render (d, s, a, setstate, setgs, i))

        Html.div [ prop.className "location-hub-window dialog-window"
                   prop.children [ DialogTextComponents.DialogtextRenderer(
                                       animation,
                                       loc.Text,
                                       s.GameState,
                                       s.GameState.Iteration
                                   )
                                   Html.div [ prop.className "variants"
                                              prop.children (
                                                  renderedVariants
                                                  @ [ Html.div [ prop.innerHtml "This is person" ] ]
                                              ) ] ] ]

    [<ReactComponent>]
    static member DialogWindowView
        (
            w: DialogViewModel,
            s: GlobalState,
            a: AnimationProgress,
            setstate: GlobalState -> unit,
            setUpdatedGameState
        ) =
        let animation =
            match s.Animation with
            | NoAnimation -> "animate__animated animate__fadeInLeft animate__faster"
            | VariantChosen (_) -> "animate__animated animate__fadeOutRight"

        let render (el: DialogVariantView) =
            match (el.IsLocked) with
            | Unlocked -> Some(Components.DialogButton, el)
            | Reason (_) -> Some(Components.LockedDialogButton, el)
            | Hidden -> None

        let setgs action actionText =
            setUpdatedGameState
            <| executeDialogStateUpdate s.GameState (toString w.Text) (w.Actor.asString ()) action actionText


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
                                                  List.choose render w.Variants
                                                  |> List.mapi (fun i (render, d) ->
                                                      render (d, s, a, setstate, setgs, i))
                                              ) ] ] ]


    [<ReactComponent>]
    static member DialogButton(prp: DialogVariantView, (s: GlobalState), a, setter, onClickAction, i) =
        let withAnimation () =
            setTimeout (fun _ -> onClickAction prp.Action prp.Text |> ignore) 250
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
                "animate__animated animate__fadeInDown animate__faster "
                + calcDelay i
            | VariantChosen (x) when x = i -> "animate__animated animate__zoomOutDown animate__faster"
            | VariantChosen (_) -> "animate__animated animate__fadeOut animate__faster"

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
    static member LockedDialogButton(prp: DialogVariantView, (s: GlobalState), a, setter, setgs, i) =
        let calcDelay i =
            if i = 0 then
                ""
            else
                sprintf "animate__delay-%ds" i

        let addClassesByAnimation =
            function
            | NoAnimation ->
                "animate__animated animate__fadeInDown animate__faster "
                + calcDelay i
            | VariantChosen (x) when x = i -> "animate__animated animate__zoomOutDown"
            | VariantChosen (_) -> "animate__animated animate__fadeOut animate__faster"

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

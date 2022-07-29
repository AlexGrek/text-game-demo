module Actions

open State
open Data

type Link =
    | OptionalLink of string
    | StaticLink of string
    | PopLink
    | PushLink of UReference
    | JumpLink of UReference
    | PushLocationLink of string
    | PushPersonLink of string
    | UnknownLink

type IAction =
    abstract member Exec : State -> State
    abstract member Links : unit -> Link list
    abstract member ComposeAfter : (State -> State) -> IAction

type NoAction() =
    interface IAction with
        member _.Exec s = s
        member _.Links() = []
        member _.ComposeAfter _ = failwith "cannot compose NoAction"

let updateStateOptional modify s =
    match modify with
    | None -> s
    | Some(m) -> m s

type ToWindowMod =
    { Target: string
      Mod: State -> State }
    interface IAction with
        member this.Links() : Link list = [ StaticLink(this.Target) ]

        member x.Exec state =
            (x.Mod >> gotoDialogWindow x.Target) state

        member x.ComposeAfter m =
            {x with Mod = m >> x.Mod} :> IAction

type ToWindow =
    { Target: string }
    interface IAction with
        member this.Links() : Link list = [ StaticLink(this.Target) ]
        member x.Exec state = gotoDialogWindow x.Target state
        member x.ComposeAfter modifier = {Target = x.Target; Mod = modifier;} :> IAction

type Pop =
    { Mod: Option<State -> State> }
    interface IAction with
        member this.Links() : Link list = [ PopLink ]

        member x.Exec state =
            state
            |> (Option.defaultValue id x.Mod >> popDialog)
        
        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with Pop.Mod = newMod } :> IAction

type PopToLocation =
    { Mod: Option<State -> State> }
    interface IAction with
        member this.Links() : Link list = [ PopLink ]

        member x.Exec state =
            state
            |> (Option.defaultValue id x.Mod >> popToLocation)
        
        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with PopToLocation.Mod = newMod } :> IAction


type Push =
    { TargetRef: UReference; Mod: Option<State -> State> }
    interface IAction with
        member this.Links() : Link list = [ PushLink(this.TargetRef) ]

        member x.Exec state =
            state 
            |> Option.defaultValue id x.Mod
            |> pushDialog x.TargetRef.D x.TargetRef.W
        
        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with Push.Mod = newMod } :> IAction

type PopPush =
    { TargetRef: UReference; Mod: Option<State -> State> }
    interface IAction with
        member this.Links() : Link list = [ PushLink(this.TargetRef) ]

        member x.Exec state =
            state 
            |> Option.defaultValue id x.Mod
            |> popDialog
            |> pushDialog x.TargetRef.D x.TargetRef.W
        
        member x.ComposeAfter m =
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with PopPush.Mod = newMod } :> IAction

// move to another dialog without pushing anything to UI stack
type Jump =
    { TargetRef: UReference; Mod: Option<State -> State> }
    interface IAction with
        member this.Links() : Link list = [ JumpLink(this.TargetRef) ]

        member x.Exec state =
            state
            |> Option.defaultValue id x.Mod
            |> toDialog x.TargetRef.D x.TargetRef.W
        
        member x.ComposeAfter m =
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with Jump.Mod = newMod } :> IAction

type PushLocation =
    { LocationRef: string; Mod: Option<State -> State> }
    interface IAction with
        member this.Links() : Link list = [ PushLocationLink(this.LocationRef) ]

        member x.Exec state =
            state 
            |> Option.defaultValue id x.Mod
            |> pushLocation x.LocationRef
        
        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with PushLocation.Mod = newMod } :> IAction

type PushPersonDialog =
    { PersonHubRef: string; Mod: Option<State -> State>; SpecificAction: IAction option }
    interface IAction with
        member this.Links() : Link list = [ PushPersonLink(this.PersonHubRef) ]

        member x.Exec state =
            let newState = 
                state
                |> Option.defaultValue id x.Mod
                |> pushPerson x.PersonHubRef
            match x.SpecificAction with
            | Some(act) ->
                act.Exec newState
            | None -> newState
        
        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with PushPersonDialog.Mod = newMod } :> IAction

type ChangeLocation =
    { LocationRef: string; Mod: Option<State -> State>; }
    interface IAction with
        member this.Links() : Link list = [ PushLocationLink(this.LocationRef) ]

        member x.Exec state =
            state 
            |> Option.defaultValue id x.Mod
            |> changeLocation x.LocationRef
        
        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with ChangeLocation.Mod = newMod } :> IAction

type PersonDecider = 
    { Person: string; Decider: State -> string; Mod: Option<State -> State>; SpecificAction: IAction option }
    interface IAction with
        member this.Links() : Link list = [ PushPersonLink(this.Person) ]

        member x.Exec state =
            let npcRef = x.Decider state
            let newState = 
                state
                |> Option.defaultValue id x.Mod
                |> pushPerson npcRef
            match x.SpecificAction with
            | Some(act) ->
                act.Exec newState
            | None -> newState

        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with PersonDecider.Mod = newMod } :> IAction

let private linksFromTwo (a: IAction) (b: IAction) =
    List.concat [ a.Links(); b.Links() ]

type Conditional =
    { Predicate: State -> bool
      OnTrue: IAction
      OnFalse: IAction }
    interface IAction with
        member this.Links() =
            linksFromTwo this.OnTrue this.OnFalse
            
        member this.Exec state =
            if (this.Predicate state) then
                this.OnTrue.Exec state
            else
                this.OnFalse.Exec state
        member this.ComposeAfter m =
            { this with 
                OnTrue = this.OnTrue.ComposeAfter m
                OnFalse = this.OnFalse.ComposeAfter m } :> IAction

type MoveWithNewStack =
    { Stack: UReference list
      Target: UReference
      Mod: Option<State -> State> }
    interface IAction with
        member this.Links() =
            List.singleton <| JumpLink(this.Target)
        member this.Exec state =
            updateStateOptional this.Mod state
            |> jumpWithDialogStackTo this.Target this.Stack
        member x.ComposeAfter m = 
            let newMod =
                match x.Mod with
                | None -> Some(m)
                | Some(old) -> Some(m >> old)
            { x with Mod = newMod } :> IAction

type OnlyOnce = 
    { Property: Props.BoolProperty; DoOnce: IAction; DoOther: IAction }
    interface IAction with
        member this.Links() = 
            linksFromTwo this.DoOnce this.DoOther
        
        member this.Exec state =
            if (this.Property.GetOrFalse state) then
                // property is true, so already executed
                this.DoOther.Exec state
            else
                // first time
                this.Property.Set true
                >> this.DoOnce.Exec <| state
        member this.ComposeAfter m =
            { this with 
                DoOnce = this.DoOnce.ComposeAfter m
                DoOther = this.DoOther.ComposeAfter m } :> IAction

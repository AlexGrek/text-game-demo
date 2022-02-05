module Dialog

open State
open RichText
open Data
open Actions

type DialogOptionLock =
    | Reason of string
    | Unlocked
    | Hidden

type DialogOptionLockFunction =
    | Locked of string * (State -> bool)
    | MayBeHidden of (State -> bool)
    | AlwaysUnlocked

type DialogVariant =
    { Text: string
      Action: IAction
      Locked: DialogOptionLockFunction }
    member x.IsLocked(s: State) =
        match x.Locked with
        | Locked (reason, func) ->
            if (func s) then
                Unlocked
            else
                Reason(reason)
        | AlwaysUnlocked -> Unlocked
        | MayBeHidden (func) ->
            if (func s) then
                Unlocked
            else
                Hidden

// dialog variant generators

let makeUnlockedVariant text action =
    { Text = text
      Action = action
      Locked = AlwaysUnlocked }

let makeLockedVariant reason predicate text action =
    { Text = text
      Action = action
      Locked = Locked(reason, predicate) }

let makeHiddenVariant predicate text action =
    { Text = text
      Action = action
      Locked = MayBeHidden(predicate) }

let toWindow target = { Target = target }

let pushWindowRef target = { TargetRef = target; Mod = None }

// the dialog itself

type TextDialogWindow =
    { Name: string
      Actor: string option
      Text: State -> RichText
      Variants: State -> DialogVariant list
      OnEntry: Option<State -> State> }

type ProxyWindow = {
    Name: string
    Action: State -> IAction
}

type DialogWindow = 
    | Proxy of ProxyWindow
    | TextWindow of TextDialogWindow
    member x.GetName() = 
        match x with
        | Proxy(p) -> p.Name
        | TextWindow(w) -> w.Name

type Dialog =
    { Name: string
      DialogWindows: Map<string, DialogWindow>
      DefaultWindow: Option<State -> UReference> }

let REPO_DIALOG = Data.GlobalRepository<Dialog>()

// static text
let stxt text = fun _ -> RichText.text text

let randomTextOf text (state: State) =
    let options = List.map parse text
    Utils.randomOfListWithSeed options state.Iteration
    <| state

let ctxt predicate text1 text2 =
    let t1 = parse text1
    let t2 = parse text2
    fun (s: State) -> if (predicate s) then t1 s else t2 s


let ptxt text = parse text

let str = text

let s x = fun _ -> x

let dialogWindow name actor textGen variants =
    { Name = name
      Actor = actor
      Text = textGen
      Variants = variants
      OnEntry = None }

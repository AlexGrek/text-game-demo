module Props

open State

let inline (-.) (x: string) (y: string) = x + "." + y

type Property(path) =
    let x = path
    // abstract Set: 'T -> State -> State
    // abstract Get: State -> 'T
    // abstract DefaultValue: 'T

    // member x.GetRaw state =
    //     state.Data.Item path

    member x.GetRawOption(state: State) =
        if (Map.containsKey path state.Data) then
            Some(state.Data.Item path)
        else
            None

    member x.SetRaw value state =
        { state with Data = (Map.change path (fun _ -> Some(value)) state.Data) }

let asBool (f: obj) = f :?> bool

type BoolProperty(path, defv) =
    inherit Property(path)
    member x.DefaultValue = defv

    member x.Set value state =
        let oValue = (value :> obj)

        let newData =
            (Map.change path (fun _ -> Some(oValue)) state.Data)

        { state with Data = newData }

    member x.Get(state: State) =
        Option.map asBool (x.GetRawOption state)
        |> Option.defaultValue x.DefaultValue

    member x.GetOr defVal state =
        Option.map asBool (x.GetRawOption state)
        |> Option.defaultValue defVal

    member x.GetOrFalse = x.GetOr false
    member x.GetOrTrue = x.GetOr true
    static member Personal name propName def = BoolProperty(name -. propName, def)

type IterationCounter(path: string) =
    inherit Property(path)
    member val IsStarted = BoolProperty(path -. "started", false)
    member x.DefaultValue = 0;
    member x.Set value state = 
        x.SetRaw(value :> obj) state
        
    member x.Start (state: State) = 
        x.Set state.Iteration state
        |> x.IsStarted.Set true
    member x.Get(state: State) =
        if (Map.containsKey path state.Data) && (x.IsStarted.Get state) then
            let o = (state.Data.Item path)
            o :?> int
        else
            state.Iteration
    member x.Elapsed (state: State) = 
        let was = x.Get state
        state.Iteration - was
    static member Personal name propName =
        IterationCounter(name -. propName)

type ListStringProperty(path: string, defv: string array) =
    inherit Property(path)
    member x.DefaultValue = defv
    member x.Set value state = x.SetRaw(value :> obj) state

    member x.Get(state: State) =
        if (Map.containsKey path state.Data) then
            let o = (state.Data.Item path)
            o :?> array<string>
        else
            x.DefaultValue

    member x.Add item state =
        let updated = Array.append (x.Get state) [| item |]
        x.Set updated state

    member x.Contains item state = Array.contains item (x.Get state)
    member x.AsList = x.Get >> Array.toList

    member x.AddNew item state =
        if (x.Contains item state |> not) then
            x.Add item state
        else
            state

    member x.RemoveOrDie item state =
        if (x.Contains item state) then
            x.RemoveIfPresent item state
        else
            failwith <| sprintf "Cannot remove item %A, found only %A" item (x.AsList state)

    member x.RemoveIfPresent item state =
        let newOne = Array.filter (fun x -> x <> item) (x.Get state)
        x.Set newOne state

    static member Personal name propName = ListStringProperty(name -. propName, [||])
    static member PersonalWithDefault name propName defv = ListStringProperty(name -. propName, Seq.toArray defv)


let asString (f: obj) = f :?> string

type StringProperty(path, defv) =
    inherit Property(path)
    member x.DefaultValue = defv

    member x.Set value state =
        let oValue = (value :> obj)

        let newData =
            (Map.change path (fun _ -> Some(oValue)) state.Data)

        { state with Data = newData }

    member x.Get(state: State) =
        Option.map asString (x.GetRawOption state)
        |> Option.defaultValue x.DefaultValue

    static member Personal name propName defv = StringProperty(name -. propName, defv)
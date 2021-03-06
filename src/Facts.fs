module Facts

open State
open DSL

let makePersonalId person id =
    person + "::" + id

// General fact IDs contains only words
let isGeneral (s: string) =
    not <| s.Contains("::")

// Personal fact IDs contains :: as separator between person ID and fact ID
let isPersonal (s: string) = 
    not <| isGeneral s

type Fact =
    { FactId: string
      Description: string
      Deniable: bool
      Name: string
      ActionOnAcquisition: Option<State -> State> }
    member x.Acquire state =
        let newState =
            match x.ActionOnAcquisition with
            | Some (m) -> m state
            | None -> state
        printfn "Accuired fact: %A %s" x.FactId x.Description
        { newState with KnownFacts = newState.KnownFacts.Add x.FactId }
    member x.IsKnown (state: State) =
        state.KnownFacts.Contains x.FactId
    member x.Deny (state: State) =
        if (x.Deniable && x.IsKnown state) then
            {state with KnownFacts = state.KnownFacts.Remove x.FactId }
        else
            if (not x.Deniable) then
                failwith <| sprintf "Trying to deny '%s', while it is not marked as deniable fact" x.FactId
            else
                state // do nothing if was unknown

type PersonFact = 
    {
      FactId: string
      PersonName: string
      Name: string
      Description: string
    }
    member x.IsKnown (state: State) =
        state.KnownPersons.ContainsKey x.PersonName && 
            state.KnownPersons.[x.PersonName].Contains x.FactId
    member x.Acquire state =
        printfn "Accuired personal fact: %A %s" x.FactId x.Description
        let newState = Person.meetPersonName x.PersonName state
        { newState with KnownPersons = Map.add x.PersonName (newState.KnownPersons.[x.PersonName] .Add(x.FactId)) newState.KnownPersons }

let REPO_FACTS = Data.GlobalRepository<Fact>()
let REPO_FACTS_PERSONAL = Data.GlobalRepository<PersonFact>()

let createFact id name desc =
    { FactId = id
      Description = desc
      Deniable = false
      Name = name
      ActionOnAcquisition = None }
    |> Data.save<Fact> REPO_FACTS id

let createPersonalFact personName id name desc =
    { FactId = id
      PersonName = personName
      Description = desc
      Name = name }
    |> Data.save<PersonFact> REPO_FACTS_PERSONAL id

let createFactDeniable id name desc =
    { FactId = id
      Description = desc
      Deniable = true
      Name = name
      ActionOnAcquisition = None }
    |> Data.save<Fact> REPO_FACTS id

let createFactWithAction id name desc action =
    { FactId = id
      Description = desc
      Name = name
      Deniable = false
      ActionOnAcquisition = Some(action) }
    |> Data.save<Fact> REPO_FACTS id
    |> ignore

let acquireFactById (fact: string) state =
    if (not <| Data.keyExists<Fact> REPO_FACTS fact) then
        failwith
        <| sprintf "Cannot acquire non-exsistent fact: %s; Facts exist: %A" fact (Data.dumpGlobalKeys<Fact> REPO_FACTS)

    let theFact = Data.getGlobal<Fact> REPO_FACTS fact
    printfn "Fact %s acquired: %A" fact theFact.Description
    theFact.Acquire state

let lookupFact id = 
    if (not <| Data.keyExists<Fact> REPO_FACTS id) then
        failwith
        <| sprintf "Cannot lookup non-exsistent fact: %s; Facts exist: %A" id (Data.dumpGlobalKeys<Fact> REPO_FACTS)
    Data.getGlobal<Fact> REPO_FACTS id
    
let listKnownFactsGeneral (state: State) =
    Seq.toList state.KnownFacts
    |> List.filter isGeneral

let listAllFacts() =
    Data.dumpGlobalKeys REPO_FACTS
    
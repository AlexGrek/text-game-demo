module Data

open System.Collections.Generic

[<StructuredFormatDisplay("{D}.{W}")>]
type UReference =
    { D: string
      W: string }
    override this.ToString() = sprintf "%s.%s" this.D this.W
    member this.IsFull() = this.D <> ""
    static member Stub w = { D = ""; W = w }
    static member Parse str = 
      let d, w = Utils.splitByDot2 str
      { D = d; W = w}

type Pointer =
    | FullPointer of UReference
    | PushPointer of UReference
    | WPointer of string
    | DPointer of string
    | BackPointer

type GlobalRepository<'a> () =
    static let instance = GlobalRepository<'a>()

    

    member val Repo = Dictionary<string, 'a>()

    member instance.Get key = instance.Repo.[key]

    member instance.ContainsKey key = instance.Repo.ContainsKey(key)

    member instance.Add key value = instance.Repo.Add(key, value)

    member instance.Keys() = Seq.toList <| instance.Repo.Keys

    member instance.Pairs () = (Seq.toList >> List.map (fun x -> (x, instance.Repo.[x]))) (instance.Repo.Keys) 

    member instance.Reset() =
      instance.Repo.Clear()




// type Repository<'a>() =
//     member private x.Repo = Dictionary<string, 'a>()

//     member x.Get key = x.Repo.[key]

//     member x.Item
//         with get (k: string) = x.Get k

//     member x.Add key value =
//         x.Repo.Add(key, value)
//         x

let dumpGlobalKeys<'a> (repo: GlobalRepository<'a>) = repo.Keys ()

let dumpGlobalPairs<'a> (repo: GlobalRepository<'a>) = repo.Pairs ()

let keyExists<'a> (repo: GlobalRepository<'a>) (key: string) = repo.ContainsKey (key)

let save<'a> (repo: GlobalRepository<'a>) key value =
    repo.Add key value
    value

let getGlobal<'a> (repo: GlobalRepository<'a>) key =
    if (repo.ContainsKey key) then
        repo.Get key
    else
        failwith
        <| sprintf "Missing global key %A\n Dump keys: %A" key (dumpGlobalKeys<'a> repo)

let resetGlobal<'a> (repo: GlobalRepository<'a>) = repo.Reset ()


type ReferenceCheckerStorage () =
    static let instance = ReferenceCheckerStorage()

    member val Repo = Set.empty with get, set
    static member Instance = instance

    static member MustAdd (reference: string) = instance.Repo <- Set.add reference instance.Repo

    static member Contains (reference: string) = Set.contains reference instance.Repo

    static member Reset () = instance.Repo <- Set.empty

    static member All () = instance.Repo


let toReferenceChecker reference =
  if ReferenceCheckerStorage.Contains reference then
    reference
  else
    ReferenceCheckerStorage.MustAdd reference
    reference

let checkReferencesAgainst (refs: UReference list) =
  let predicate (r: string) (l: UReference) =
    l.W = r
  let check (r: string) = 
    match List.tryFind (predicate r) refs with
    | None -> Some(r)
    | Some(_) -> None
  let errors = Set.toList (ReferenceCheckerStorage.All()) |> List.choose check
  if (List.isEmpty >> not) errors then
    failwith <| sprintf "Failed references: %A" errors



let resetAll () =
  ReferenceCheckerStorage.Reset ()
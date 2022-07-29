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

type GlobalRepository<'a> () =
    static let instance = GlobalRepository<'a>()

    

    member val Repo = Dictionary<string, 'a>()

    member instance.Get key = instance.Repo.[key]

    member instance.ContainsKey key = instance.Repo.ContainsKey(key)

    member instance.Add key value = instance.Repo.Add(key, value)

    member instance.AddOrChange key value = instance.Repo.[key] <- value

    member instance.Keys() = Seq.toList <| instance.Repo.Keys

    member instance.Vals() = instance.Repo.Values

    member instance.Pairs () = (Seq.toList >> List.map (fun x -> (x, instance.Repo.[x]))) (instance.Repo.Keys) 

    member instance.Reset() =
      instance.Repo.Clear()

let dumpGlobalKeys<'a> (repo: GlobalRepository<'a>) = repo.Keys ()

let dumpGlobalPairs<'a> (repo: GlobalRepository<'a>) = repo.Pairs ()

let values<'a> (repo: GlobalRepository<'a>) = repo.Vals ()

let keyExists<'a> (repo: GlobalRepository<'a>) (key: string) = repo.ContainsKey (key)

let save<'a> (repo: GlobalRepository<'a>) key value =
    repo.Add key value
    value

let getGlobal<'a> (repo: GlobalRepository<'a>) key =
    if (repo.ContainsKey key) then
        repo.Get key
    else
        failwith
        <| sprintf "Missing global key '%A'\n Dump keys: %A" key (dumpGlobalKeys<'a> repo)

let patch<'a> (repo: GlobalRepository<'a>) func key =
    if (repo.ContainsKey key) then
        let prev = repo.Get key
        let updated = func prev
        repo.AddOrChange key updated
    else
        failwith
        <| sprintf "Missing global key '%A'\n Dump keys: %A" key (dumpGlobalKeys<'a> repo)

let getOrNone<'a> (repo: GlobalRepository<'a>) key =
    if (repo.ContainsKey key) then
      Some(repo.Get key)
    else
      None

let resetGlobal<'a> (repo: GlobalRepository<'a>) = repo.Reset ()

let resetAll () =
  ()
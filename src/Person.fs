module Person

open RoleModel
open Data
open State

type Person(name: string, dispalyName: string) =
    abstract member Roles: unit -> RoleModel
    abstract member DisplayName: State -> string
    default _.DisplayName (state: State) = dispalyName
    default _.Roles() = RoleModel([])
    member val Name = name
    member val DefaultDisplayName = dispalyName

let IN_LOCATION_ID = "inLocation"

type InLocation(name: string, defaultLocation: string) =
    inherit Role(IN_LOCATION_ID)
    member val CurrentLocation = Props.StringProperty.Personal name "currentLocation" defaultLocation

let asInLocation (p: Person) =
    Option.map (fun (x: Role) -> x :?> InLocation) (p.Roles().AsOption IN_LOCATION_ID)

let REPO_PERSONS = GlobalRepository<Person>()

let savePers (p: Person) =
    save REPO_PERSONS p.Name p

// was person met before or any fact about person known (by name)
let doesKnowPersonName (p: string) (s: State) =
    s.KnownPersons.ContainsKey p

// make person known
let meetPersonName (p: string) s =
    if (doesKnowPersonName p s) then
        s
    else
        {s with KnownPersons = s.KnownPersons.Add (p, Set.empty)}
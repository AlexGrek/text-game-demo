module Person

open RoleModel
open Data
open State

type Person(name: string) =
    abstract member Roles: unit -> RoleModel
    abstract member DisplayName: State -> string
    default _.DisplayName (s: State) = name + "_UNNAMED"
    default _.Roles() = RoleModel([])
    member val Name = name

let IN_LOCATION_ID = "inLocation"

type InLocation(name: string, defaultLocation: string) =
    inherit Role(IN_LOCATION_ID)
    member val CurrentLocation = Props.StringProperty.Personal name "currentLocation" defaultLocation

let asInLocation (p: Person) =
    Option.map (fun (x: Role) -> x :?> InLocation) (p.Roles().AsOption IN_LOCATION_ID)

let REPO_PERSONS = GlobalRepository<Person>()

let savePers (p: Person) =
    save REPO_PERSONS p.Name p

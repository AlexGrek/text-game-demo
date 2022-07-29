module VisualNovelChars

open Props
open NPC
open Person
open Location

type LocationMap() =
    let street = createRootLocation "street" { 
                pic "locs/street.jpg"
            }
    let home = createLocation street "home" {
            pic "locs/home.jpg"
            icon "locs/home_icon.jpg"
        }

    member val Street = street
    member val Home = home
        

type Myself() =
    inherit Person("myself", "Подсознание")
    let name = "myself"

    member val Naked = BoolProperty.Personal name "naked" true
    member val HighAsFuck = BoolProperty.Personal name "high" false
    member val IphoneTaken = BoolProperty.Personal name "iphoneTaken" false
    member val IsSporty = BoolProperty.Personal name "sporty" false
    member x.WearSport = x.IsSporty.Set true >> x.Naked.Set false

    member x.WearOffice =
        x.IsSporty.Set false >> x.Naked.Set false

    member x.GetNaked =
        x.IsSporty.Set false >> x.Naked.Set true

let makePersonPropNameFromName (s: string) =
    s.ToLower().Replace(" ", "-")

let DEBUG_ROLE_ID = "debugrole"

type DebugRole(name: string, someText: string, beautyVal: int) =
    inherit RoleModel.Role(DEBUG_ROLE_ID)
    member val Beauty =
        IntProperty.Personal name "beauty" beautyVal
    static member Cast (p: Person) =
        (p.Roles().As DEBUG_ROLE_ID) :?> DebugRole

type DebugGirl(displayName, beauty: int, map: LocationMap) =
    inherit Person(makePersonPropNameFromName displayName, displayName)
    
    let unknownName = "Unknown(" + displayName + ")"
    override x.Roles() = 
        RoleModel.RoleModel([
            InLocation(x.Name, map.Home)
            Talker(x.Name, DefaultBasicAnswers, DontCare)
            DebugRole(x.Name, "some text", beauty)
        ])
    member x.AsDebug() =
        DebugRole.Cast x
    

type World(facts: VisualNovelFacts.NovelFacts) =
    let name = "world"
    member val GameStarted = BoolProperty.Personal name "gameStarted" false

let saved (pers: 'a) = 
    savePers (pers :> Person) |> ignore
    pers

type Chars(facts: VisualNovelFacts.NovelFacts) =
    let map = LocationMap()
    member val Map = map

    member val GloriaDebug = saved <| DebugGirl("Gloria Anderson", 4, map)
    member val EmberDebug = saved <| DebugGirl("Ember Jovani", 3, map)

    member val Myself = saved <| Myself()
    member val World = World(facts)

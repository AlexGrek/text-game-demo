module Location

open Data

type Location = {
    Name: string;
    ChildLinks: Location list
    Parent: Location option
    PathToPictureDefault: string option
    PathToIcon: string option
}

let REPO_LOCATIONS = GlobalRepository<Location>()

type LocationBuilder(name: string, root: Location option) =
    member __.Yield(_) : Location =
        { Name = name
          ChildLinks = []
          Parent = root
          PathToIcon = None
          PathToPictureDefault = None }

    member __.Run(a: Location) : Location =
        match root with
        | Some(loc) -> 
            let nameOfRoot = loc.Name
            patch 
                REPO_LOCATIONS 
                (fun obj -> {obj with ChildLinks = a :: obj.ChildLinks})
                nameOfRoot
        | None -> ()
        save<Location> REPO_LOCATIONS name a

    [<CustomOperation("external_link")>]
    member __.Lnk(l: Location, toLink: Location) : Location =
        { l with ChildLinks = toLink :: l.ChildLinks }

    [<CustomOperation("pic")>]
    member __.Pic(l: Location, pic: string) : Location =
        { l with PathToPictureDefault = Some(pic) }

    [<CustomOperation("icon")>]
    member __.Icon(l: Location, pic: string) : Location =
        { l with PathToIcon = Some(pic) }

let createLocation root name =
    LocationBuilder(name, Some(root))

let createRootLocation name =
    LocationBuilder(name, None)

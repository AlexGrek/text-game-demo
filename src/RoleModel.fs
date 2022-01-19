module RoleModel

type Role(roleIdentifier: string, name: string) =
    let x = name
    member x.RoleId =
        roleIdentifier

type RoleModel(rolesList: Role list) =
    let roles = 
        List.map (fun (r: Role) -> (r.RoleId, r)) rolesList 
        |> Map.ofList
    member x.Roles() = 
        roles
    member x.CanBe roleId =
        roles.ContainsKey roleId
    member x.CanBe (role: Role) =
        roles.ContainsKey (role.RoleId)
    member x.As (roleId: string) =
        roles.[roleId]
    
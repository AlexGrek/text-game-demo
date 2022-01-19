module RoleModel

type Role(roleIdentifier: string) =
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
    member x.As (role: Role) =
        roles.[role.RoleId]
    member x.AsOption (roleId: string) =
        if (x.CanBe roleId) then
            Some(roles.[roleId])
        else
            None
    member x.AsOption (role: Role) =
        if (x.CanBe role) then
            Some(roles.[role.RoleId])
        else
            None
    
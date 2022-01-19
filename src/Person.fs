module Person

open RoleModel

type Person(name: string) =
    abstract member Roles: unit -> RoleModel
    default x.Roles() = RoleModel([])



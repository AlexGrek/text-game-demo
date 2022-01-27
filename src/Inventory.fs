module Inventory

open State
open Actions
open RoleModel
open Data

type ItemClass =
    | Consumable
    | GameItem

type Item = {
    Name: string
    DisplayName: string
    OnPick: IAction option
    OnUse: IAction option
    Class: ItemClass
}

let makeConsumable name dname onConsume =
    { 
        Name = name
        DisplayName = dname;
        OnPick = None
        OnUse = Some(onConsume)
        Class = Consumable
    }

let makeGameItem name dname onPick onUse =
    {
        Name = name
        DisplayName = dname
        OnPick = onPick
        OnUse = onUse
        Class = GameItem
    }

let HAS_INVENTORY_ID = "inventory"

type HasInventory(name: string, initialItems: string array) =
    inherit Role(HAS_INVENTORY_ID)
    member val Inventory = Props.ListStringProperty.PersonalWithDefault name "currentLocation" initialItems
    member x.HasItem item (s: State) =
        Array.contains item (x.Inventory.Get s)
    member x.Take item (s: State) =
        x.Inventory.Add item s
    member x.TakeOne item (s: State) =
        x.Inventory.AddNew item s
    member x.Drop item (s: State) =
        x.Inventory.RemoveOrDie item s



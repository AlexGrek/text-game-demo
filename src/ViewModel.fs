module ViewModel

open RichText
open LocationHub

type DialogActorView =
    | UnknownActor of string
    // todo: add known actor with link to person

type DialogViewModel =
    {
        Actor: DialogActorView
        Text: RichText
        Variants: Dialog.DialogVariant
    }

type LocaitionHubViewModel = {
    Text: RichText
    DisplayName: string
    Variants: Dialog.DialogVariant
    Design: HubDesign.HubDesign
    Locations: LocationHubVariant list
    Persons: LocationHubVariant list
}

type UIView = 
    | DialogView of DialogViewModel
    | LocaitionHubView of LocaitionHubViewModel

type View = {
    Facts: Facts.Fact list
    UI: UIView
}
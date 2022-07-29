module LocationHub

open Data
open RichText
open State
open Dialog
open HubDesign

type LocationHubVariant = {
    Pic: string option
    Variant: DialogVariant
}

type LocationHub = {
    Locations: LocationHubVariant list
    Persons: State -> LocationHubVariant list
    Variants: State -> DialogVariant list
    Description: State -> RichText
    Name: string
    Design: HubDesign
}

let REPO_LOCATION_HUBS = GlobalRepository<LocationHub>()

let makeLocationVariant var pic =
    {Pic = Some(pic); Variant = var}

let makePicturelessLocationVariant var =
    {LocationHubVariant.Pic = None; Variant = var}
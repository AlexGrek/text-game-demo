module LocationHub

open Data
open RichText
open State
open Dialog

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
}

let REPO_LOCATIONS = GlobalRepository<LocationHub>()

let makeLocationVariant var pic =
    {Pic = Some(pic); Variant = var}

let makePicturelessLocationVariant var =
    {LocationHubVariant.Pic = None; Variant = var}
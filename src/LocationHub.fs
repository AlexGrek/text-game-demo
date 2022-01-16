module LocationHub

open Data
open RichText
open State
open Dialog

type LocationVariant = {
    Pic: string
    Variant: DialogVariant
}

type PersonTarget = {
    Pic: string
    Variant: DialogVariant
}

type LocationHub = {
    Locations: LocationVariant list
    Persons: State -> PersonTarget list
    Variants: State -> DialogVariant list
    Description: State -> RichText
}

let REPO_LOCATIONS = Data.GlobalRepository<LocationHub>()
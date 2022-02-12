module PersonHub

open Data
open RichText
open State
open Dialog
open HubDesign

type PersonHubVariant = {
    Pic: string option
    Variant: DialogVariant
}

type PersonHub = {
    StartingDialog: State -> Actions.Jump option
    Variants: State -> DialogVariant list
    Description: State -> RichText
    Name: string
    FactsDialogLink: string
    Design: HubDesign
}

let REPO_PERSON_HUBS = GlobalRepository<PersonHub>()

let makePersonVariant var pic =
    {Pic = Some(pic); Variant = var}

let makePicturelessPersonVariant var =
    {PersonHubVariant.Pic = None; Variant = var}
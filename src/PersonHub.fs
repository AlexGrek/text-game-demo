module PersonHub

open Data
open RichText
open State
open Dialog
open HubDesign


type AllowedInteractions = 
    {
        AllowedTalkAbout: bool
        AllowedGive: bool
    }
    with 
        static member All = 
            {AllowedTalkAbout = true; AllowedGive = true} 
        static member Nothing =
            {AllowedTalkAbout = false; AllowedGive = false}
        static member OnlyTalk =
            {AllowedInteractions.Nothing with AllowedTalkAbout = true} 

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
    Allowed: State -> AllowedInteractions
    Design: HubDesign
    ExitVariant: DialogVariant
}

let REPO_PERSON_HUBS = GlobalRepository<PersonHub>()

let makePersonVariant var pic =
    { Pic = Some(pic); Variant = var }

let makePicturelessPersonVariant var =
    { PersonHubVariant.Pic = None; Variant = var }
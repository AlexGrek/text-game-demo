module Serialication

open Fable.SimpleJson

let private MAIN_STATE_KEY = "state_dump"

let saveToLocalStorage key value =
    Browser.WebStorage.localStorage.setItem (key, value)

let stringifyState =
    Json.serialize<State.State>

let saveMainState =
    saveToLocalStorage MAIN_STATE_KEY << stringifyState

let loadFromLocalStorage =
    Browser.WebStorage.localStorage.getItem

let deserializeState = Json.parseAs<State.State>

let loadMainState() =
    loadFromLocalStorage MAIN_STATE_KEY |> deserializeState
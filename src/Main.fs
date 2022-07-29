module Main

open Feliz
open App
open Browser.Dom
open Fable.Core.JsInterop

importSideEffects "./styles/global.scss"

VisualNovelGame.initThis()

ReactDOM.render(
    Components.HelloWorld(),
    document.getElementById "feliz-app"
)
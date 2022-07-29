module VisualNovelGame

open DSL
open NPC
open State
open VisualNovelFacts

// Remember to add all chapters here!
let init(facts: NovelFacts, chars: VisualNovelChars.Chars) =
    Chapter01.init(facts, chars)
    Chapter00Debug.init(facts, chars)

let INITIAL_STATE =
    makeInitialStateInDialog { D = "init"; W = "init" }

let build () =
    Engine.reset ()
    let facts = VisualNovelFacts.NovelFacts()
    let chars = VisualNovelChars.Chars(facts)

    init(facts, chars)

type VisualNovelRunner() =
    interface Engine.IGameRunner with
        member _.InitialState() = INITIAL_STATE
        member _.Build() = build ()

let initThis () =
    let runner = VisualNovelRunner()
    Engine.setGameRunner runner


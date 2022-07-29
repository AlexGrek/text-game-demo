module Chapter00Debug

open DSL
open Props
open NPC
open Dialog

let init(facts: VisualNovelFacts.NovelFacts, chars: VisualNovelChars.Chars) =
    let debugBoolProp = BoolProperty("debug00", false)
    createDialog "debug" [
        window "00" {
            stxt "Debug mode: you can test UI here"
            var (popVariant "BACK TO GAME")
            var ("go to non-existent dialog" --- "nonexistentdialog.somewindow")
            var ("window with random text" -- "randomTextWindow")
            var ("chars" --- "propsPlayground.ch")
            var (locked "always" (fun s -> false) "locked always" {
                action doPop
            })
            var (locked "conditional" debugBoolProp.Get "locked conditional" {
                action (doGoToWindow "00")
                modify (debugBoolProp.Set false)
            })
            var (variant "enable bool prop" {
                action (doGoToWindow "00")
                modify (debugBoolProp.Set true)
            })
            var ("to entry tests" --- "sample.00")
            var ("to fact tests" --- "facts.00")
            var ("to props tests" --- "propsPlayground.00")
        }
        window "randomTextWindow" {
            rand ["Random entry 01"; "Random entry 02"; "Random entry 03"]
            var ("home" -- "00")
        }
    ] |> ignore

    let shorttext = "this is short text"
    let longtext = """
        This is long text, very fucking long text. You can read it and feel pain because
        it is soooo long, has no meaning at all. Fuck that, I quit, I don't want to read that anymore. 
        Please, please, stop this text. Oh no. Oh my fucking god, it still has no meaning.
    """
    let longTextEnabled = Props.BoolProperty("debugLongText", false)

    createDialog "sample" [
        window "00" {
            stxt "this is sample window with some entries"
            var ("to 1 entries" -- "to1")
            var ("to 2 entries" -- "to2")
            var ("to 3 entries" -- "to3")
            var ("to 4 entries" -- "to4")
            var ("to 5 entries" -- "to5")
            var (hidden (longTextEnabled.Get >> not) "switch to long text" {
                modify (longTextEnabled.Set true)
                action (doGoToWindow "00")
            })
            var (hidden (longTextEnabled.Get) "switch to short text" {
                modify (longTextEnabled.Set false)
                action (doGoToWindow "00")
            })
            var (popVariant "back")
        }
        window "to1" {
            ctxt 
                longTextEnabled.Get
                longtext
                shorttext
            var ("home" -- "00")
        }
        window "to2" {
            ctxt 
                longTextEnabled.Get
                longtext
                shorttext
            var ("to 2 entries" -- "to2")
            var ("home" -- "00")
        }
        window "to3" {
            ctxt 
                longTextEnabled.Get
                longtext
                shorttext
            var ("to 3 entries" -- "to3")
            var ("to 2 entries" -- "to2")
            var ("home" -- "00")
        }
        window "to4" {
            ctxt 
                longTextEnabled.Get
                longtext
                shorttext
            var ("to 3 entries" -- "to3")
            var ("to 2 entries" -- "to2")
            var ("to 4 entries" -- "to4")
            var ("home" -- "00")
        }
        window "to5" {
            ctxt 
                longTextEnabled.Get
                longtext
                shorttext
            var ("to 3 entries" -- "to3")
            var ("to 2 entries" -- "to2")
            var ("to 4 entries" -- "to4")
            var ("to 5 entries" -- "to5")
            var ("home" -- "00")
        }
    ] |> ignore

    createDialog "facts" [
        window "00" {
            ctxt facts.debugFact.IsKnown "Fact is known" "Fact is not known yet"
            var (variant "know fact" {
                action (doGoToWindow "00")
                modify (facts.debugFact.Acquire)
            })
            var (popVariant "back")
        }
    ] |> ignore


    let cntr = IntProperty("debugProp", 0)
    let msg = StringProperty("msg", "")
    createDialog "propsPlayground" [
        window "00" {
            ptxt ("Counter value: *(" + cntr.GetPath() + ")*. So...")
            var (variant "increase counter" {
                action (doGoToWindow "00")
                modify (cntr.Inc)
            })
            var (variant "decrease counter" {
                action (doGoToWindow "00")
                modify (cntr.Dec)
            })
            var (variant "window that should inc" {
                action (doGoToWindow "incWindow")
            })
            var (popVariant "back")
            var ("to chars test" -- "ch")
        }
        window "incWindow" {
            stxt "This window must have increased the counter, go back and check!"
            onEntry (cntr.Inc)
            var ("go back" -- "00")
        }

        window "ch" {
            ptxt "Chars test. Beauty of girls is *(msg)*."
            onEntry (fun s -> msg.Set ((chars.EmberDebug.AsDebug().Beauty.Get s)) s)
            var ("some words with Ember" --- "debugember.00")
            var (npcDialogVariant "talk to Gloria" chars.GloriaDebug)
            var (npcDialogVariant "talk to Ember" chars.EmberDebug)
            var (popVariant "back")
            var ("to chars test" -- "ch")
        }
    ] |> ignore

    createDialog "debugember" [
        windowWithPerson "00" chars.EmberDebug {
            stxt "Oh... Hello..."
            var ("I love you" -- "love")
            var ("I hate you" -- "hate")
            var (popVariant "exit")
        }
    ] |> ignore

    let initialGloria =
        npc chars.GloriaDebug "init" {
            allow PersonHub.AllowedInteractions.OnlyTalk
            stxt "Hi, I am Gloria, a Debug girl!"
        }

    let initialEmber =
        npc chars.EmberDebug "init" {
            allow PersonHub.AllowedInteractions.All
            stxt "Hi, I am Debug girl Ember, nice to meet you!"
        }

    personConfig chars.GloriaDebug {
        name (s "Gloria, a Debug girl")
        hub initialGloria
    } |> ignore

    personConfig chars.EmberDebug {
        name (s "Ember Debug")
        hubName "init"
    } |> ignore

module VisualNovelFacts

open Facts

type NovelFacts() =

    member val started =
        createFact "started" "Игра начата" """Это первый ФАКТ, который говорит о том, что игра началась
        """

    member val debugFact =
        createFact "debugfact" "Debug fact" """!! This is debug fact for UI testing, user 
        should not see it"""
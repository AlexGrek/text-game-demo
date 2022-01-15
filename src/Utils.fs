[<RequireQualifiedAccess>]
module Utils

let splitByDot2 (str: string) =
    match str.Split('.') with
    | [| s1; s2|] -> (s1, s2)
    | _ -> failwith <| "cannot split string into 2 by dot: '" + str + "'"

let random = System.Random()

let randomOfList lst =
    printfn "Choosing random of: %A" lst
    let max = List.length lst
    let rand = random.Next max
    printfn "Chosen %d" rand
    lst.[rand]

let randomOfListWithSeed lst seed =
    let random = System.Random(seed)
    printfn "Choosing random of: %A" lst
    let max = List.length lst
    let rand = random.Next max
    printfn "Chosen %d" rand
    lst.[rand]
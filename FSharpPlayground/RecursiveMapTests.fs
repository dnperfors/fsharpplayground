module FSharpPlayground.RecursiveMapTests

open Expecto
open Swensen.Unquote

let map func list =
    let rec mapImpl func = function
        | [] -> []
        | head :: tail -> func head :: mapImpl func tail

    mapImpl func list

let rev list =
    let rec revImpl acc = function
        | [] -> acc
        | head :: tail -> revImpl (head :: acc) tail
    revImpl [] list
    

[<Tests>]
let tests = [
    testCase "Map" <| fun _ ->
        test <@ map (fun x -> x * x) [1..5] = [1; 4; 9; 16; 25] @>
        let result = map id [1..1000000] 
        test <@ result = [1..1000000] @>

    testCase "Rev" <| fun _ ->
        test <@ rev [1..5] = [5..1] @>
]
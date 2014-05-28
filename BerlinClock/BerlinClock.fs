module BerlinClock

let clock time =
    let Yellow = "Y"
    let Off = "O"

    let toInt (x: char) = System.Int32.Parse(string x)

    let formatSeconds seconds =
        match (toInt seconds) % 2 with
        | 0 -> Yellow
        | _ -> Off

    let formatRow3 minute =
        match (toInt minute) / 5 with
        | 0 -> "OOOOOOOOOOO"
        | _ -> "YOOOOOOOOOO"

    let formatRow4 minute =
        let underFiveMinutes = (toInt minute) % 5
        seq {
            yield! Seq.init underFiveMinutes (fun _ -> Yellow)
            yield! Seq.init (4 - underFiveMinutes) (fun _ -> Off)
        }
        |> Seq.reduce (+)

    match time |> List.ofSeq with
    | [_; _; ':'; minuteTens; minute; ':'; _; second] -> sprintf "%s\nOOOO\nOOOO\n%s\n%s" (second |> formatSeconds) (minute |> formatRow3) (minute |> formatRow4)

open Xunit
open Xunit.Extensions
open FsUnit.Xunit

[<Theory>]
[<InlineData("00:00:00", "Y\nOOOO\nOOOO\nOOOOOOOOOOO\nOOOO")>]
[<InlineData("00:00:01", "O\nOOOO\nOOOO\nOOOOOOOOOOO\nOOOO")>]
[<InlineData("00:00:02", "Y\nOOOO\nOOOO\nOOOOOOOOOOO\nOOOO")>]
[<InlineData("00:01:00", "Y\nOOOO\nOOOO\nOOOOOOOOOOO\nYOOO")>]
[<InlineData("00:03:00", "Y\nOOOO\nOOOO\nOOOOOOOOOOO\nYYYO")>]
[<InlineData("00:05:00", "Y\nOOOO\nOOOO\nYOOOOOOOOOO\nOOOO")>]
[<InlineData("00:07:00", "Y\nOOOO\nOOOO\nYOOOOOOOOOO\nYYOO")>]
[<InlineData("00:10:00", "Y\nOOOO\nOOOO\nYYOOOOOOOOO\nOOOO")>]
let correctClock (input, expected)=
    input
    |> clock
    |> should equal expected

[<Fact>]
let pokeNCrunch ()=
    true |> should equal true

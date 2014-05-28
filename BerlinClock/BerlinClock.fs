module BerlinClock

let clock time =
    let Yellow = "Y"
    let Off = "O"

    let toInt (x: char) = System.Int32.Parse(string x)

    let formatSeconds seconds =
        match (toInt seconds) % 2 with
        | 0 -> Yellow
        | _ -> Off

    let formatRow4 minute =
        let underFiveMinutes = (toInt minute) % 5
        seq {
            yield! Seq.init underFiveMinutes (fun _ -> Yellow)
            yield! Seq.init (4 - underFiveMinutes) (fun _ -> Off)
        }
        |> Seq.reduce (+)

    match time |> List.ofSeq with
    | [_; _; _; _; minute; _; _; second] -> sprintf "%s\nOOOO\nOOOO\nOOOOOOOOOOO\n%s" (second |> formatSeconds) (minute |> formatRow4)

open Xunit
open FsUnit.Xunit

[<Fact>]
let Zero ()=
    "00:00:00"
    |> clock
    |> should equal "Y\nOOOO\nOOOO\nOOOOOOOOOOO\nOOOO"

[<Fact>]
let oneSecond ()=
    "00:00:01"
    |> clock
    |> should equal "O\nOOOO\nOOOO\nOOOOOOOOOOO\nOOOO"

[<Fact>]
let twoSecond ()=
    "00:00:02"
    |> clock
    |> should equal "Y\nOOOO\nOOOO\nOOOOOOOOOOO\nOOOO"

[<Fact>]
let oneMinute ()=
    "00:01:00"
    |> clock
    |> should equal "Y\nOOOO\nOOOO\nOOOOOOOOOOO\nYOOO"

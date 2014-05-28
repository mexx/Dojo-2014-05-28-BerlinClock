module BerlinClock

let clock time =
    let Red = "R"
    let Yellow = "Y"
    let Off = "O"

    let toInt (x: char) = System.Int32.Parse(string x)

    let formatSeconds seconds =
        match (toInt seconds) % 2 with
        | 0 -> Yellow
        | _ -> Off

    let format modulo on number =
        let count = number % modulo
        seq {
            yield! Seq.init count on
            yield! Seq.init (modulo - count - 1) (fun _ -> Off)
        }
        |> Seq.reduce (+)

    let formatRow1 (hourTens, hour) =
        ((toInt hourTens) * 10 + (toInt hour)) / 5
        |> format 5 (fun _ -> Red)

    let formatRow2 hour =
        toInt hour
        |> format 5 (fun _ -> Red)

    let formatRow3 (minuteTens, minute) =
        ((toInt minuteTens) * 10 + (toInt minute)) / 5
        |> format 12 (fun index -> if (index + 1) % 3 = 0 then Red else Yellow)

    let formatRow4 minute =
        toInt minute
        |> format 5 (fun _ -> Yellow)

    match time |> List.ofSeq with
    | [hourTens; hour; ':'; minuteTens; minute; ':'; _; second] ->
        sprintf "%s\n%s\n%s\n%s\n%s"
            (second |> formatSeconds)
            ((hourTens, hour) |> formatRow1)
            (hour |> formatRow2)
            ((minuteTens, minute) |> formatRow3)
            (minute |> formatRow4)

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
[<InlineData("00:15:00", "Y\nOOOO\nOOOO\nYYROOOOOOOO\nOOOO")>]
[<InlineData("01:00:00", "Y\nOOOO\nROOO\nOOOOOOOOOOO\nOOOO")>]
[<InlineData("05:00:00", "Y\nROOO\nOOOO\nOOOOOOOOOOO\nOOOO")>]

[<InlineData("13:17:01", "O\nRROO\nRRRO\nYYROOOOOOOO\nYYOO")>]
[<InlineData("23:59:59", "O\nRRRR\nRRRO\nYYRYYRYYRYY\nYYYY")>]
[<InlineData("24:00:00", "Y\nRRRR\nRRRR\nOOOOOOOOOOO\nOOOO")>]
let correctClock (input, expected)=
    input
    |> clock
    |> should equal expected

[<Fact>]
let pokeNCrunch ()=
    true |> should equal true

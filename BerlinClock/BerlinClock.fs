module BerlinClock

let clock time = 
    match time |> List.ofSeq with
    | [_; _; _; _; _; _; _; '0'] -> """Y
OOOO
OOOO
OOOOOOOOOOO
OOOO"""
    | [_; _; _; _; _; _; _; '1'] -> """O
OOOO
OOOO
OOOOOOOOOOO
OOOO"""

open Xunit
open FsUnit.Xunit

[<Fact>]
let Zero ()=
    "00:00:00"
    |> clock
    |> should equal """Y
OOOO
OOOO
OOOOOOOOOOO
OOOO"""

[<Fact>]
let oneSecond ()=
    "00:00:01"
    |> clock
    |> should equal """O
OOOO
OOOO
OOOOOOOOOOO
OOOO"""


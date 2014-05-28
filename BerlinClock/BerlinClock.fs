module BerlinClock

let clock _ = """Y
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
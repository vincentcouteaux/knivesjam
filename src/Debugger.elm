module Debugger exposing (..)
import Generator exposing (..)

vmin = 16
vmax = 40

closest pitch note =
    let
        oct = pitch//12 - 1
        below = note2pitch (oct-1) note
        center = note2pitch oct note
        above = note2pitch (oct+1) note
        b_gap = abs (below-pitch)
        c_gap = abs (center-pitch)
        a_gap = abs (above-pitch)
        min3 = min b_gap (min c_gap a_gap)
        closestnote = if min3 == b_gap then below else if min3 == c_gap then center else above
    in
        if closestnote < vmin then closestnote + 12 else if closestnote > vmax then closestnote-12 else closestnote


cumsumlist l i res =
    case l of
        [] -> res
        h::t -> cumsumlist t (h+i) ((h+i)::res)

cumsum l i =
    case l of
        [] -> []
        h::t -> (h+i)::(cumsum t (h+i))

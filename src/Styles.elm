module Styles exposing (..)
import Generator as G
import JazzDrums
import JazzPiano
import JazzBass
import BossaDrums
import BossaBass
import BossaPiano
import FusionDrums
import FusionBass
import Random as R
import Tune

type Style = Swing | Bop | Bossa | Fusion

getSeqGenerator : Style -> G.ChordProg -> Int -> R.Generator (Tune.Sequence)
getSeqGenerator s =
    G.mergeSeqGenerators <|
    case s of
        Bossa -> [ BossaDrums.sequenceGenerator
                 , BossaBass.sequenceGenerator
                 , BossaPiano.sequenceGenerator ]
        Fusion -> [ FusionDrums.sequenceGenerator
                  , FusionBass.sequenceGenerator
                  , BossaPiano.sequenceGenerator ]
        _ -> [ JazzBass.sequenceGenerator
             , JazzDrums.sequenceGenerator
             , JazzPiano.sequenceGenerator ]

str2style : String -> Style
str2style s =
    case s of
        "Bossa" -> Bossa
        "Fusion" -> Fusion
        "Bop" -> Bop
        _ -> Swing

style2str : Style -> String
style2str s =
    case s of
        Swing -> "Swing"
        Bop -> "Bop"
        Bossa -> "Bossa"
        Fusion -> "Fusion"


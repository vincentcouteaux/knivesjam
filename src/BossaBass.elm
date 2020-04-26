module BossaBass exposing (..)

import Tune
import Random as R
import Generator as G
import JazzBass as Jb
import Dict exposing (Dict)

bass2Seq : Dict Int G.Note -> Tune.Sequence
bass2Seq dic =
    List.concatMap 
        (\(k, v) -> [ Tune.Event (toFloat k) True (G.note2pitch 1 v) "bass" 1
                 , Tune.Event (toFloat (k+2)) False (G.note2pitch 1 v) "bass" 1 ]
        )
        (Dict.toList dic)

sequenceGenerator : G.ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator cp _ =
    Jb.addPump (Jb.fondaOnChange cp) cp 2
    |> Jb.dictGen2GenDict
    |> R.map bass2Seq
    |> R.map (\l -> (Tune.Event cp.end False 40 "bass" 1)::l)
    |> R.map (List.filter (\e -> e.time <= cp.end))

module JazzDrums exposing (..)

import Tune

chabada : Float -> Tune.Sequence
chabada end =
    List.range 0 (floor ((end-1)/4))
    |> List.map ((*) 4)
    |> List.concatMap
        (\i -> let fb = toFloat i in
               [ Tune.Event fb True 2 "drums" 0.5
               , Tune.Event (fb + 1) True 2 "drums" 0.5
               , Tune.Event (fb + 1) True 3 "drums" 1
               , Tune.Event (fb + 1.66) True 2 "drums" 0.7
               , Tune.Event (fb + 2) True 2 "drums" 0.5
               , Tune.Event (fb + 3) True 2 "drums" 0.5
               , Tune.Event (fb + 3) True 3 "drums" 1 
               , Tune.Event (fb + 3.66) True 2 "drums" 0.7 ])

drumseq : Float -> Tune.Sequence
drumseq end = (Tune.Event 0 True 5 "drums" 1)::(chabada end)

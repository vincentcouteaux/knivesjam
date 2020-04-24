module BossaDrums exposing (..)

import Tune
import Random as R
import Generator as G

bossagroove : Float -> Tune.Sequence
bossagroove end = 
    List.range 0 (floor ((end-1)/8))
    |> List.map ((*) 8)
    |> List.concatMap
        (\i -> let fb = toFloat i in
               [ Tune.Event fb True 1 "drums" 0.5
               , Tune.Event (fb + 1.5) True 1 "drums" 0.5
               , Tune.Event (fb + 3) True 1 "drums" 0.5
               , Tune.Event (fb + 5) True 1 "drums" 0.5
               , Tune.Event (fb + 6.5) True 1 "drums" 0.5
               , Tune.Event (fb + 1) True 3 "drums" 1
               , Tune.Event (fb + 3) True 3 "drums" 1 
               , Tune.Event (fb + 5) True 3 "drums" 1
               , Tune.Event (fb + 7) True 3 "drums" 1
               , Tune.Event (fb + 0) True 2 "drums" 0.5
               , Tune.Event (fb + 1) True 2 "drums" 0.5
               , Tune.Event (fb + 2) True 2 "drums" 0.5
               , Tune.Event (fb + 3) True 2 "drums" 0.5 
               , Tune.Event (fb + 4) True 2 "drums" 0.5 
               , Tune.Event (fb + 5) True 2 "drums" 0.5
               , Tune.Event (fb + 6) True 2 "drums" 0.5
               , Tune.Event (fb + 7) True 2 "drums" 0.5
               , Tune.Event (fb + 0.5) True 2 "drums" 0.5
               , Tune.Event (fb + 1.5) True 2 "drums" 0.5
               , Tune.Event (fb + 2.5) True 2 "drums" 0.5
               , Tune.Event (fb + 3.5) True 2 "drums" 0.5 
               , Tune.Event (fb + 4.5) True 2 "drums" 0.5 
               , Tune.Event (fb + 5.5) True 2 "drums" 0.5
               , Tune.Event (fb + 6.5) True 2 "drums" 0.5
               , Tune.Event (fb + 7.5) True 2 "drums" 0.5
               , Tune.Event fb True 0 "drums" 0.5
               , Tune.Event (fb + 7.5) True 0 "drums" 0.5
               , Tune.Event fb True 4 "drums" 0.5
               , Tune.Event (fb + 3.5) True 0 "drums" 0.5
               ])
    |> List.filter
        (\e -> e.time <= end)
    |> (::) (Tune.Event end False 2 "drums" 1)

bossagrooveCrash : Float -> Tune.Sequence
bossagrooveCrash end = (Tune.Event 0 True 5 "drums" 1)::(bossagroove end)

sequenceGenerator : G.ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator cp _ =
    R.constant (bossagrooveCrash cp.end)

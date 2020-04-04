module JazzDrums exposing (..)

import Tune
import Random as R
import Generator as G

chabada : Float -> Int -> Tune.Sequence
chabada end beatsPerBar =
    List.range 0 (floor ((end-1)/(toFloat beatsPerBar)))
    |> List.map ((*) beatsPerBar)
    |> List.concatMap
        (\i -> let fb = toFloat i in
               [ Tune.Event fb True 2 "drums" 0.5
               , Tune.Event (fb + 1) True 2 "drums" 0.5
               , Tune.Event (fb + 1) True 3 "drums" 1
               , Tune.Event (fb + 1.66) True 2 "drums" 0.7
               , Tune.Event (fb + 2) True 2 "drums" 0.5 ]
               ++ (if beatsPerBar > 3 then
                   ([ Tune.Event (fb + 3) True 2 "drums" 0.5
                   , Tune.Event (fb + 3) True 3 "drums" 1 
                   , Tune.Event (fb + 3.66) True 2 "drums" 0.7 ]
                   ++ (if beatsPerBar > 4 then
                       [ Tune.Event (fb + 3) True 2 "drums" 0.5]
                       else []
                      )
                   )
                   else []
                  )
        )

randomShots : Float -> Int -> R.Generator (List (Maybe Float))
randomShots p n =
    R.list n 
    <| R.andThen 
         (\f -> if f<p 
                then R.map (\f2 -> Just f2) (R.float 0.3 1)
                else R.constant Nothing) 
         (R.float 0 1)

listShots2seq : List (Maybe Float) -> Int -> Tune.Sequence
listShots2seq l pitch =
    let
        r1 = List.range 0 (List.length l) |> List.map toFloat
        r2 = List.map ((+) 0.66) r1
        onset = List.map2 (\a b -> [a, b]) r1 r2 |> List.concat
        zip = List.map2 Tuple.pair onset l
    in 
        List.filterMap 
            (\(o, m) -> 
                Maybe.map
                    (\g -> Tune.Event o True pitch "drums" g)
                    m
            )
            zip

chabadaCrash : Float -> Int -> Tune.Sequence
chabadaCrash end beatsPerBar = (Tune.Event 0 True 5 "drums" 1)::(chabada end beatsPerBar)

sequenceGenerator : G.ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator cp beatsPerBar =
    let n = floor (cp.end)
        shotGen = randomShots 0.2 (2*n)
        snare = R.map (\l -> listShots2seq l 1) shotGen
        kick = R.map (\l -> listShots2seq l 0) shotGen
    in
        R.map3 (\s1 s2 s3 -> s1++s2++s3) snare kick (R.constant (chabadaCrash cp.end beatsPerBar))


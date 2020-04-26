module FusionBass exposing (..)

import Tune
import Random as R
import Generator as G
import FusionDrums exposing (unifl)

type alias ShortEvt =
   { time: Float
   , duration: Float
   , octave: Bool
   , velocity: Float }

possibleRhythms : List (List ShortEvt)
possibleRhythms =
    [ [ ShortEvt 0.05 0.5 False 0.6, ShortEvt 0.55 0.5 False 1
      , ShortEvt 1.05 0.5 False 0.6, ShortEvt 1.55 0.5 False 1
      , ShortEvt 3.75 0.25 False 0.6 ] 
    , [ ShortEvt 0.05 0.5 False 0.6, ShortEvt 0.55 0.5 False 1
      , ShortEvt 1.05 0.5 False 0.6, ShortEvt 1.55 0.15 False 0.6
      , ShortEvt 1.75 0.15 True 1, ShortEvt 3.75 0.25 True 1 ] 
    , [ ShortEvt 0.05 0.5 False 0.6, ShortEvt 0.55 0.5 False 1
      , ShortEvt 1.05 0.5 True 0.6, ShortEvt 1.55 0.15 False 0.6
      , ShortEvt 1.75 0.15 False 1, ShortEvt 3.75 0.25 True 1 ] 
    , [ ShortEvt 0.05 0.5 False 0.6, ShortEvt 0.55 0.5 False 1
      , ShortEvt 1.05 0.5 True 0.6, ShortEvt 1.55 0.5 False 0.6
      , ShortEvt 2.05 0.9 False 1, ShortEvt 3.05 0.8 False 1 ] ]

--possibleRhythms : List (List Float)
--possibleRhythms = --[ [ 0.05, 1.05, 2.05, 3.05 ] ]
--                  [ [ 0.05, 0.55, 1, 1.55 ]
--                  , [ 0.05, 0.55, 1, 1.55, 3.55 ]
--                  , [ 0.05, 1, 2, 2.75]
--                  ]

--randomBarAt : Int -> Float -> R.Generator (List (Float, Bool))
randomBarAt : Int -> Float -> R.Generator (List ShortEvt)
randomBarAt sig start =
    let sig_ = toFloat sig in
    unifl possibleRhythms
    |> R.map (List.filter (\e -> e.time <= sig_))
    |> R.map (List.map (\e -> { e | time=e.time + start}))
    --|> R.andThen
    --    ((List.map 
    --        (\e -> R.map2 
    --            (\c b -> (c,b)) 
    --            (R.constant e) 
    --            (R.weighted (0.2, False) [(0.8, True)])))
    --    >> G.listGen2GenList)

allBars : Float -> Int -> R.Generator (List ShortEvt)
allBars end sig =
    List.range 0 (floor ((end-1)/(toFloat sig)))
    |> List.map ((*) sig)
    |> List.map toFloat
    |> List.map (randomBarAt sig)
    |> G.listGen2GenList
    |> R.map List.concat
    |> R.map (List.filter (\e -> e.time + e.duration <= end))

sequenceGenerator : G.ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator cp sig =
    allBars cp.end sig
    |> R.map (
        (List.concatMap (\{time, duration, octave, velocity} -> 
            let 
                note = G.getBass (G.getChordAt cp time)
                p = n2pitch octave note
            in
            [ Tune.Event time True p "bass" velocity
            , Tune.Event (time+duration) False p "bass" velocity ]
            )
        ))
    |> R.map (List.filter (\e -> e.time <= cp.end))
    |> R.map (List.filter (\e -> (G.getChordAt cp (e.time+0.2)).type_ /= G.NA))
       
n2pitch : Bool -> G.Note -> Int
n2pitch oct note =
    G.note2pitch 0 note
    |> (\p -> if p < 16 then p + 12 else p)
    |> (\p -> if oct then p + 12 else p)

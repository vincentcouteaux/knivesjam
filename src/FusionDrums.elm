module FusionDrums exposing (..)

import Tune
import Random as R
import Generator as G

possibleKicks : List Tune.Sequence
possibleKicks =
    [ [ Tune.Event 0 True 0 "drums" 1
      , Tune.Event 2 True 0 "drums" 1 ]
    , [ Tune.Event 0 True 0 "drums" 1
      , Tune.Event 3.5 True 0 "drums" 1 ]
    , [ Tune.Event 0 True 0 "drums" 1
      , Tune.Event 3.5 True 0 "drums" 1 ]
    , [ Tune.Event 0 True 0 "drums" 1
      , Tune.Event 1.75 True 0 "drums" 1
      , Tune.Event 2.5 True 0 "drums" 1 ]
    , [ Tune.Event 0 True 0 "drums" 1
      , Tune.Event 1.5 True 0 "drums" 1
      , Tune.Event 2 True 0 "drums" 1 ]
    ]

possibleSnares : List Tune.Sequence
possibleSnares = 
    [ [ Tune.Event 1 True 1 "drums" 1
      , Tune.Event 3 True 1 "drums" 1 ]
    , [ Tune.Event 1 True 1 "drums" 1
      , Tune.Event 1.75 True 1 "drums" 0.2
      , Tune.Event 3 True 1 "drums" 1 ]
    , [ Tune.Event 1 True 1 "drums" 1
      , Tune.Event 3 True 1 "drums" 1
      , Tune.Event 3.25 True 1 "drums" 0.2
      , Tune.Event 3.75 True 1 "drums" 0.2 ]
    , [ Tune.Event 1 True 1 "drums" 1
      , Tune.Event 2.25 True 1 "drums" 0.2
      , Tune.Event 3 True 1 "drums" 1
      , Tune.Event 3.75 True 1 "drums" 0.2 ]
    , [ Tune.Event 1 True 1 "drums" 1
      , Tune.Event 1.25 True 1 "drums" 0.2
      , Tune.Event 2.25 True 1 "drums" 0.2
      , Tune.Event 3 True 1 "drums" 1
      , Tune.Event 3.75 True 1 "drums" 0.2 ]
    , [ Tune.Event 0.75 True 1 "drums" 1
      , Tune.Event 3 True 1 "drums" 1 ]
    ]

possibleRide : List Tune.Sequence
possibleRide =
    [ [ Tune.Event 0 True 2 "drums" 0.5
      , Tune.Event 0.5 True 2 "drums" 0.5
      , Tune.Event 1 True 2 "drums" 0.5
      , Tune.Event 1.5 True 2 "drums" 0.5
      , Tune.Event 2 True 2 "drums" 0.5
      , Tune.Event 2.5 True 2 "drums" 0.5
      , Tune.Event 3 True 2 "drums" 0.5
      , Tune.Event 3.5 True 2 "drums" 0.5 ]
    , [ Tune.Event 0 True 2 "drums" 0.5
      , Tune.Event 0.5 True 2 "drums" 0.5
      , Tune.Event 1 True 2 "drums" 0.5
      , Tune.Event 1.5 True 2 "drums" 0.5
      , Tune.Event 2 True 2 "drums" 0.5
      , Tune.Event 2.5 True 2 "drums" 0.5
      , Tune.Event 3 True 2 "drums" 0.5
      , Tune.Event 3.5 True 2 "drums" 0.5
      , Tune.Event 3.75 True 2 "drums" 0.5 ]
    ]

possibleHh : Tune.Sequence
possibleHh = 
    [ Tune.Event 0 True 3 "drums" 0.5
    , Tune.Event 1 True 3 "drums" 0.5
    , Tune.Event 2 True 3 "drums" 0.5
    , Tune.Event 3 True 3 "drums" 0.5 ]

unifl : List (List a) -> R.Generator (List a)
unifl l =
    case l of
        [] -> R.constant []
        h::t -> R.uniform h t

randomBar : Int -> R.Generator Tune.Sequence
randomBar sig = 
    let sig_ = toFloat sig in 
    G.listGen2GenList 
        [ unifl possibleKicks
        , unifl possibleSnares
        , unifl possibleRide
        , R.constant possibleHh ]
    |> R.map List.concat
    |> R.map
        (List.filter (\e -> e.time < sig_))

delay : Float -> Tune.Event -> Tune.Event
delay s e = { e | time=e.time + s }

randomBarAt : Int -> Float -> R.Generator Tune.Sequence
randomBarAt sig start =
    randomBar sig
    |> R.map (List.map (delay start))

sequenceGenerator : G.ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator cp sig =
    List.range 0 (floor ((cp.end-1)/(toFloat sig)))
    |> List.map ((*) sig)
    |> List.map toFloat
    |> List.map (randomBarAt sig)
    |> G.listGen2GenList
    |> R.map List.concat
    |> R.map ((::) (Tune.Event 0 True 5 "drums" 1))
    |> R.map ((::) (Tune.Event cp.end False 0 "drums" 1))
    |> R.map (List.filter (\e -> e.time <= cp.end))

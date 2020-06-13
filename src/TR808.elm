module TR808 exposing (..)

import Tune
import Random as R
import Generator as G
import FusionBass exposing (ShortEvt, metaSequenceGenerator)
import FusionDrums exposing (unifl, delay)

type HhEvent = Hh Float Float | Cicada Float
type Shot = Shot Float Float
type Bass = Root | Octave | Fifth
type Kick = Kick Float Bass

hhBars : List (List HhEvent)
hhBars = [ [ Hh 0 0.7, Hh 1 1, Hh 2 0.7, Hh 3 1
           , Hh 4 0.7, Hh 5 1, Hh 6 0.7, Hh 7 1
           , Hh 8 0.7, Hh 9 1, Hh 10 0.7, Hh 11 1
           , Hh 12 0.7, Hh 13 1, Hh 14 0.7, Hh 15 1 ]
         , [ Hh 0 0.7, Hh 1 1, Hh 2 0.7, Cicada 3
           , Hh 4 0.7, Hh 5 1, Hh 6 0.7, Hh 7 1
           , Hh 8 0.7, Hh 9 1, Hh 10 0.7, Hh 11 1
           , Hh 12 0.7, Hh 13 1, Hh 14 0.7, Hh 15 1 ]
         , [ Hh 0 0.7, Hh 1 1, Hh 2 0.7, Hh 3 1
           , Hh 4 0.7, Hh 5 1, Hh 6 0.7, Cicada 7
           , Hh 8 0.7, Hh 9 1, Hh 10 0.7, Hh 11 1
           , Hh 12 0.7, Cicada 13, Hh 14 0.7, Cicada 15 ]
         , [ Hh 0 0.7, Hh 0.6666 1,  Hh 1.3333 0.7, Hh 2 0.7, Hh 2.6666 1, Hh 3.3333 1
           , Hh 4 0.7, Cicada 5, Hh 6 0.7, Hh 7 1
           , Hh 8 0.7, Hh 9 1, Hh 10 0.7, Hh 11 1
           , Hh 12 0.7, Cicada 13, Hh 14 0.7, Cicada 15 ]
        ]

snareBars : List (List Shot)
snareBars = [ [ Shot 4 1, Shot 12 1 ]
            , [ Shot 4 1, Shot 12 1, Shot 13.5 0.7, Shot 15.5 0.7 ]
            , [ Shot 4 1, Shot 7.5 0.6, Shot 12 1 ]
            , [ Shot 4 1, Shot 4.5 0.6, Shot 5.5 0.6, Shot 7 0.4, Shot 12 1 ]
            ]

kickBars : List (List Kick)
kickBars = [ [ Kick 0 Root, Kick 7.5 Root ]
           , [ Kick 0 Root, Kick 7.5 Octave,  Kick 8 Root ]
           , [ Kick 0 Root, Kick 4 Fifth,  Kick 7 Octave, Kick 8 Root ]
           , [ Kick 0 Root, Kick 7 Octave,  Kick 8 Root, Kick 14 Fifth, Kick 15 Octave ]
           ]

--randomHhBar : R.Generator (List HhEvent)
--randomHhBar fb =

hhBarToSeq : List HhEvent -> Tune.Sequence
hhBarToSeq =
    List.concatMap
        (\evt -> case evt of
            Hh on vol -> [Tune.Event on True 2 "tr808" vol]
            Cicada on ->
                List.range 0 6
                |> List.map toFloat
                |> List.map (\i -> Tune.Event (on + i/7) True 2 "tr808" ((0.8)^i))
        )

snareBarToSeq : List Shot -> Tune.Sequence
snareBarToSeq = List.map
    (\(Shot on vol) -> Tune.Event on True 1 "tr808" vol)

randomBarHhSnare : R.Generator Tune.Sequence
randomBarHhSnare =
    G.listGen2GenList
        [ unifl hhBars |> R.map hhBarToSeq
        , unifl snareBars |> R.map snareBarToSeq ]
    |> R.map List.concat

delayMulti : Float -> Float -> Tune.Event -> Tune.Event
delayMulti m d e = { e | time=(e.time + d)/m }

randomBarHhSnareAt : Float -> Float -> R.Generator Tune.Sequence
randomBarHhSnareAt multi start =
    randomBarHhSnare
    |> R.map (List.map (delayMulti multi start))

--kickBar2Seq : 


sequenceGenerator : Float -> G.ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator bpm cp sig =
    let 
        multi = if bpm > 150 then 1 else
                if bpm > 80 then 2 else 4
    in
    List.range 0 (floor ((cp.end-1)*multi/16))
    |> List.map ((*) 16)
    |> List.map toFloat
    |> List.map (randomBarHhSnareAt multi)
    |> G.listGen2GenList
    |> R.map List.concat
    |> R.map ((::) (Tune.Event cp.end False 0 "drums" 1))
    |> R.map (List.filter (\e -> e.time <= cp.end))

bassSeqGen : Float -> G.ChordProg -> Int -> R.Generator (Tune.Sequence)
bassSeqGen bpm cp sig =
    let 
        multi = if bpm > 170 then 1 else
                if bpm > 120 then 2 else 4
    in
    List.range 0 (floor ((cp.end-1)*multi/16))
    |> List.map ((*) 16)
    |> List.map toFloat
    |> List.map ((*) (1/multi))
    |> List.map
        (\start -> unifl kickBars
                |> R.map (List.map
                    (\(Kick time bass) ->
                        let 
                            realtime = time/multi + start
                            chord = G.getChordAt cp (realtime + 0.5)
                            pitch = b2pitch bass chord
                        in
                        Tune.Event realtime True pitch "bass808" 1
                    )
                )
        )
    |> G.listGen2GenList
    |> R.map List.concat
    |> R.map (List.filter (\e -> e.time <= cp.end))


b2pitch : Bass -> G.Chord -> Int
b2pitch b c =
    case b of
        Root -> G.note2pitch 1 (G.getBass c)
        Fifth -> G.note2pitch 1 (G.getFifthOf c)
        Octave -> G.note2pitch 2 (G.getBass c)

module JazzPiano exposing (..)
import Tune
import Generator as G
import Random as R
import Dict exposing (Dict)

sweep : Tune.Sequence
sweep = 
    List.range 0 30
    |> List.concatMap 
        (\i -> [ Tune.Event (toFloat i) True (i+50) "piano" 1
               , Tune.Event ((toFloat i)+0.5) False (i+50) "piano" 1 ])

type alias Voicing = (Int, List (G.Chord -> G.Note)) --the int is the low pitch
type alias PossibleVoicings = G.ChordType -> List Voicing

compact : Voicing
compact = (53, [ G.getThirdOf, G.getFifthOf, G.getSeventhOf, G.getNinethOf ] )

compact13 : Voicing
compact13 = (53, [ G.getThirdOf, G.getThirteenthOf, G.getSeventhOf, G.getNinethOf ] )

all_compact : PossibleVoicings
all_compact c = if G.chordClass c == G.Dominant then [ compact13 ] else [ compact ]

possibleBars : Int -> List (List (Float, Float))
possibleBars signature = 
    case signature of
        4 -> 
            [ [ (0,0.33), (1.66,1.33) ]
            , [ (0.66, 0.33), (2,1.33) ]
            , [ (0.66, 0.33), (2.66, 0.33) ]
            , [ (0,2), (2,2) ] ]
        3 -> [ [ (0.66, 0.33), (2,1) ]
             , [ (0.66, 0.33), (2,1) ]
             , [ (1,1) ]
             , [ (1,0.33) ]
             , [ (2,1) ]
             ]
        _ -> [ [ (0.66, 0.33), (2,1), (4, 0.33) ] ]

genRhythm : Int -> Float -> R.Generator (List (Float, Float))
genRhythm signature end =
    let
        barsTemplates = possibleBars signature
        pickbar = unif2 [] barsTemplates
        addbar genlist idbar =
            let
                pickbarmap = R.map (\l -> List.map (\(o,d) -> (o+(toFloat (idbar*signature)),d)) l) pickbar
            in
                R.map2 (\gl pbm -> gl ++ pbm) genlist pickbarmap
    in
        List.foldl
            (\i out -> addbar out i) (R.constant []) (List.range 0 ((floor end)//signature))
            -- TODO filter onsets after the end

getAbove : Int -> G.Note -> Int
getAbove l n =
    let n0 = G.note2pitch 0 n in
        12*(ceiling ((toFloat (l-n0))/12)) + n0

--filterNA : List (Float, Float) -> G.ChordProg -> List (Float, Float)
--filterNA l cp =
--    List.filter
--        (\(onset, _) -> (G.getChordAt cp (onset+0.4)).type_ /= G.NA)

populateRhythm : List (Float, Float) -> G.ChordProg -> R.Generator (List { onset: Float, duration: Float, chord: List Int, volume: Float })
populateRhythm l cp =
    List.filter
        (\(onset, _) -> (G.getChordAt cp (onset+0.4)).type_ /= G.NA)
        l
    |> List.map
        (\(onset, duration) ->
            let
                chord = G.getChordAt cp (onset+0.4)
                voicing = unif2 (0,[]) (all_compact (chord.type_))
                notes =
                    R.map
                        (\(lowest, funcs) ->
                            List.map (\f -> f chord) funcs
                            |> List.map (\n -> getAbove lowest n)
                        )
                        voicing
                volume = R.float 0.05 0.2
            in
                { onset = onset, duration = duration, chord = notes, volume = volume }
        )
    |> List.filter
        (\{onset, duration} -> onset+duration <= cp.end)
    |> List.map
        (\{onset, duration, chord, volume } ->
            R.map2
                (\c  v -> {onset=onset, duration=duration, chord=c, volume=v})
                chord
                volume
        )
    |> G.listGen2GenList

sequenceGenerator : G.ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator cp signature =
    genRhythm signature cp.end
    |> R.andThen (\lff -> populateRhythm lff cp)
    |> R.map
        (\l ->
            List.concatMap
                (\{onset, duration, chord, volume} ->
                    List.concatMap
                        (\p ->
                            [ Tune.Event onset True p "piano" volume
                            , Tune.Event (onset+duration) False p "piano" 0.1 ])
                        chord
                )
                l
        )

unif2 : a -> List a -> R.Generator a
unif2 a la = R.weighted (0,a) (List.map (\x -> (1,x)) la)

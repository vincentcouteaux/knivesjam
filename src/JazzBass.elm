module JazzBass exposing (..)

import Generator exposing (..)
import Random as R
import Dict exposing (Dict)
import Tune
import Tuple3 as T3

type alias BassLine = (Dict Int (R.Generator Note))

bassline2seq : Int -> Int -> Dict Int Note -> Tune.Sequence
bassline2seq vmin vmax dic =
    let
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

    in
        Dict.toList dic |>
        List.sortBy Tuple.first |>
        List.foldl
            (\(k, v) l -> case l of
                [] -> [(k, note2pitch 1 v)]
                h::_ -> (k, closest (Tuple.second h) v)::l)
            []
        |> List.concatMap
            (\(k, p) -> [ Tune.Event (toFloat k) True p "bass" 1
                        , Tune.Event (toFloat (k+1)) False p "bass" 1 ]
            )

basslineToSequence : Dict Int Note -> Tune.Sequence
basslineToSequence = bassline2seq 16 40



basslineToSequence_old : Dict Int Note -> Tune.Sequence
basslineToSequence_old dic =
    List.concatMap 
        (\(k, v) -> [ Tune.Event (toFloat k) True (note2pitch 1 v) "bass" 1
                 , Tune.Event (toFloat (k+1)) False (note2pitch 1 v) "bass" 1 ]
        )
        (Dict.toList dic)

fondaOnChange : ChordProg -> BassLine
fondaOnChange cp =
    List.foldl
        (\evt bassline ->
            Dict.insert (floor evt.time) (R.constant (evt.chord.note)) bassline
        )
        Dict.empty
        cp.chords

addPump : BassLine -> ChordProg -> Int -> BassLine
addPump bl cp signature =
    let
        do2bars bl_ from =
            insertNoReplace 
                from 
                (R.constant (getChordAt cp (toFloat from)).note)
                <| insertNoReplace 
                        (from + signature)
                        (R.constant (getFifthOf (getChordAt cp (toFloat (from + signature)))))
                        bl_
        doRec bl_ from =
            if (toFloat from) >= cp.end then bl_ else doRec (do2bars bl_ from) (from + 2*signature)
    in
        doRec bl 0

barFillArray : Int -> { second : Chord -> Note, third : Chord -> Note, fourth : Chord -> Note}
barFillArray n =
    case n of
        0 -> { second = getNinethOf, third = getThirdOf, fourth = getNinethOf }
        1 -> { second = getSeventhOf, third = .note, fourth = getNinethOf }
        2 -> { second = getThirdOf , third = getFifthOf , fourth = getSeventhOf }
        3 -> { second = getSemitonesOf 1, third = getNinethOf , fourth = getThirdOf }
        4 -> { second = getFifthOf , third = getThirdOf , fourth = getSeventhOf }
        _ -> { second = getSeventhOf, third = getFifthOf, fourth = getNinethOf }

fillBar : ChordProg -> Int -> R.Generator ((Int, Note), (Int, Note), (Int, Note))
fillBar cp from =
    let
        funcGen = R.map barFillArray (R.int 0 5)
        curChord = getChordAt cp (toFloat from)
    in
        R.map (\funcs -> ( (from + 1, funcs.second curChord)
                         , (from + 2, funcs.third curChord)
                         , (from + 3, funcs.fourth curChord)))
              funcGen

fillBarsRec : ChordProg -> Int -> List (R.Generator ((Int, Note), (Int, Note), (Int, Note)))
fillBarsRec cp signature =
    let
        doRec l from =
            if (toFloat from) >= cp.end then l else doRec ((fillBar cp from)::l) (from + signature)
    in
        doRec [] 0

chromatism : Note -> R.Generator Note
chromatism n =
    let
        above = (note2pitch 0 n) + 1 |> pitch2note
        below = (note2pitch 0 n) - 1 |> pitch2note
    in
        R.map (\flip -> if flip == 0 then above else below) (R.int 0 1)

addApproach : BassLine -> Int -> BassLine
addApproach bl cplen =
    let
        addbefore k v newdic =
            insertNoReplace
                (modBy cplen (k-1))
                (v |> R.andThen (\n -> chromatism n))
                newdic
    in
        Dict.foldl
            addbefore
            bl
            bl
            

listGen2GenList : List (R.Generator a) -> R.Generator (List a)
listGen2GenList l =
    List.foldl
        (\gen list -> R.map2 (\g l_ -> g::l_) gen list)
        (R.constant [])
        l

dictGen2GenDict : (Dict comparable (R.Generator b)) -> R.Generator (Dict comparable b) 
dictGen2GenDict d =
    let
        l = List.map (\(k, v) -> ( R.pair (R.constant k) v)) <| Dict.toList d
    in
        R.map Dict.fromList <| listGen2GenList l


addFill : Dict Int (R.Generator Note) -> List (R.Generator ((Int, Note), (Int, Note), (Int, Note))) -> R.Generator (Dict Int Note)
addFill bl listgen = 
    let
        l = List.map (\(k, v) -> ( R.pair (R.constant k) v)) <| Dict.toList bl -- List (R.Generator (Int, Note))
        f1 = listGen2GenList l -- R.Generator (List (Int, Note))
        f2 = List.foldl
                (\gen list -> R.map2 (\g3 l_ -> (T3.first g3)::(T3.second g3)::(T3.third g3)::l_) gen list)
                f1
                listgen
    in
        R.map Dict.fromList f2

bassLineGenerator : ChordProg -> Int -> R.Generator (Dict Int Note)
bassLineGenerator cp signature =
    let
        basslineNoFill = 
            addApproach 
                (addPump (fondaOnChange cp) cp signature)
                (floor cp.end)
    in
        addFill basslineNoFill (fillBarsRec cp signature)

sequenceGenerator : ChordProg -> Int -> R.Generator (Tune.Sequence)
sequenceGenerator cp signature =
    bassLineGenerator cp signature
    |> R.map basslineToSequence

bbbass = bassLineGenerator blueBossa 4


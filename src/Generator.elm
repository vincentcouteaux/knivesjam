module Generator exposing (..)

{-|  Functions and types to manipulate elements of music theory.
-}

import Random as R
import Dict exposing (Dict)
import Tune
import Tuple3 as T3
import Random as R

type NoteName = A | B | C | D | E | F | G
type Alteration = Natural | Flat | Sharp
type alias Note =  { name: NoteName, alt: Alteration }
type ChordType = Dom7 | Min7 | Maj7
                 | Alt7 | Dom7b9 | Dom7s5 | Sus4
                 | Min7b5 | Dim | MinMaj | Maj7s5 | NA
type alias Chord = { note: Note, type_ : ChordType, bass: Maybe Note }

type alias ChordProg = { chords: List { time: Float, chord: Chord },
                         end: Float }

type ChordClass = Minor | Major | Dominant
chordClass : ChordType -> ChordClass
chordClass c = case c of
    Dom7 -> Dominant
    Alt7 -> Dominant
    Dom7b9 -> Dominant
    Dom7s5 -> Dominant
    Sus4 -> Dominant
    Min7 -> Minor
    Min7b5 -> Minor
    Dim -> Minor
    MinMaj -> Minor
    _ -> Major

chord : NoteName -> Alteration -> ChordType -> Chord
chord n a t = Chord (Note n a) t Nothing

getChordAt : ChordProg -> Float -> Chord
getChordAt cp time =
    let
        sortedChords =
            List.sortBy .time cp.chords
        chInSorted sortList ch =
            case sortList of
                [] -> ch
                h::t -> if h.time > time then ch else chInSorted t h.chord
    in
        case sortedChords of
            [] -> chord C Natural NA
            h::t -> chInSorted t h.chord

note2pitch : Int -> Note -> Int
note2pitch oct n =
    let 
        base =
            case n.name of
                C -> 0
                D -> 2
                E -> 4
                F -> 5
                G -> 7
                A -> 9
                B -> 11
        alt = case n.alt of
            Natural -> 0
            Flat -> -1
            Sharp -> 1
    in
        12 * (oct + 1) + base + alt

pitch2note : Int -> Note
pitch2note n = 
    case modBy 12 n of
        0 -> Note C Natural
        1 -> Note D Flat
        2 -> Note D Natural
        3 -> Note E Flat
        4 -> Note E Natural
        5 -> Note F Natural
        6 -> Note F Sharp
        7 -> Note G Natural
        8 -> Note A Flat
        9 -> Note A Natural
        10 -> Note B Flat
        _ -> Note B Natural

getSemitonesOf : Int -> Chord -> Note
getSemitonesOf s c = (note2pitch 0 c.note) + s |> pitch2note

getFifthOf : Chord -> Note
getFifthOf c = case c.type_ of
    Min7b5 -> getSemitonesOf 6 c
    Dim -> getSemitonesOf 6 c
    Maj7s5 -> getSemitonesOf 8 c
    Dom7s5 -> getSemitonesOf 8 c
    Alt7 -> getSemitonesOf 8 c
    _ -> getSemitonesOf 7 c

getThirdOf : Chord -> Note
getThirdOf c = case c.type_ of
    Min7 -> getSemitonesOf 3 c
    Min7b5 -> getSemitonesOf 3 c
    Dim -> getSemitonesOf 3 c
    MinMaj -> getSemitonesOf 3 c
    Sus4 -> getSemitonesOf 5 c
    _ -> getSemitonesOf 4 c

getNinethOf : Chord -> Note
getNinethOf c = case c.type_ of
    Alt7 -> getSemitonesOf 3 c
    Dom7b9 -> getSemitonesOf 3 c
    _ -> getSemitonesOf 2 c

getSeventhOf : Chord -> Note
getSeventhOf c = case c.type_ of
    Maj7 -> getSemitonesOf 11 c
    Dim -> getSemitonesOf 11 c
    MinMaj -> getSemitonesOf 11 c
    Maj7s5 -> getSemitonesOf 11 c
    _ -> getSemitonesOf 10 c

getEleventhOf : Chord -> Note
getEleventhOf c = case c.type_ of
    Dom7 -> getSemitonesOf 6 c
    Alt7 -> getSemitonesOf 6 c
    Dom7b9 -> getSemitonesOf 6 c
    Dom7s5 -> getSemitonesOf 6 c
    Maj7s5 -> getSemitonesOf 6 c
    _ -> getSemitonesOf 5 c


getThirteenthOf : Chord -> Note
getThirteenthOf c = case c.type_ of
    Alt7 -> getSemitonesOf 8 c
    Min7b5 -> getSemitonesOf 8 c
    _ -> getSemitonesOf 9 c

getBass : Chord -> Note
getBass c = case c.bass of
    Nothing -> c.note
    Just b -> b

blueBossa = ChordProg
                [ { time=0, chord=chord C Natural Min7 }
                , { time=8, chord=chord F Natural Min7 }
                , { time=16, chord=chord D Natural Min7b5 }
                , { time=20, chord=chord G Natural Alt7 }
                , { time=24, chord=chord C Natural Min7 }
                , { time=32, chord=chord E Flat Min7 }
                , { time=36, chord=chord A Flat Dom7 }
                , { time=40, chord=chord D Flat Maj7 }
                , { time=48, chord=chord D Natural Min7b5 }
                , { time=52, chord=chord G Natural Alt7 }
                , { time=56, chord=chord C Natural Min7 }
                , { time=60, chord=chord D Natural Min7b5 }
                , { time=62, chord=chord G Natural Alt7 }
                ]
                64

mergeSeqGenerators : List (ChordProg -> Int -> R.Generator (Tune.Sequence)) -> ChordProg -> Int -> R.Generator (Tune.Sequence)
mergeSeqGenerators l cp signature =
    List.foldl
        (\f gen -> 
            let curgen = f cp signature in
            R.map2 (\seq1 seq2 -> seq1++seq2) curgen gen)
        (R.constant [])
        l

--- UTILS

insertNoReplace : comparable -> v -> Dict comparable v -> Dict comparable v
insertNoReplace k v d =
    if Dict.member k d then d else (Dict.insert k v d)

listGen2GenList : List (R.Generator a) -> R.Generator (List a)
listGen2GenList l =
    List.foldl
        (\gen list -> R.map2 (\g l_ -> g::l_) gen list)
        (R.constant [])
        l

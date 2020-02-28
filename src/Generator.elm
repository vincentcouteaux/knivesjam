module Generator exposing (..)
import Random as R
import Dict exposing (Dict)
import Tune

type NoteName = A | B | C | D | E | F | G
type Alteration = Natural | Flat | Sharp
type alias Note =  { name: NoteName, alt: Alteration }
type ChordType = Dom7 | Min7 | Maj7
                 | Alt7 | Dom7b9 | Dom7s5
                 | Min7b5 | Dim | MinMaj | Maj7s5 | NA
type alias Chord = { note: Note, type_ : ChordType }

type alias ChordProg = { chords: List { time: Float, chord: Chord },
                         end: Float }

chord : NoteName -> Alteration -> ChordType -> Chord
chord n a t = Chord (Note n a) t

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

getFifthOf : Chord -> Note
getFifthOf c = case c.type_ of
    Min7b5 -> (note2pitch 0 c.note) + 6 |> pitch2note
    Dim -> (note2pitch 0 c.note) + 6 |> pitch2note
    Maj7s5 -> (note2pitch 0 c.note) + 8 |> pitch2note
    Dom7s5 -> (note2pitch 0 c.note) + 8 |> pitch2note
    Alt7 -> (note2pitch 0 c.note) + 8 |> pitch2note
    _ -> (note2pitch 0 c.note) + 7 |> pitch2note

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

--- UTILS
insertNoReplace : comparable -> v -> Dict comparable v -> Dict comparable v
insertNoReplace k v d =
    if Dict.member k d then d else (Dict.insert k v d)
--- GENERATE WALKING BASS
type alias BassLine = (Dict Int (R.Generator Note))

basslineToSequence : Dict Int Note -> Tune.Sequence
basslineToSequence dic =
    List.concatMap 
        (\(k, v) -> [ Tune.Event (toFloat k) True (note2pitch 1 v) "sineBuf" 1
                 , Tune.Event (toFloat (k+1)) False (note2pitch 1 v) "sineBuf" 1 ]
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

bbbass = dictGen2GenDict 
        <| addApproach 
            (addPump (fondaOnChange blueBossa) blueBossa 4)
            (floor blueBossa.end)

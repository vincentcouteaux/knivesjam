module Generator exposing (..)
import Random as R
import Dict exposing (Dict)
import Tune
import Tuple3 as T3

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

bbbass = bassLineGenerator blueBossa 4


module PlayerPage exposing (..)

{-| Contains the data modeling & processing utils of the Player Page.
-}

import Generator as G
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tune
import Random as R
import Dict exposing (Dict)

import Styles exposing (getSeqGenerator, Style, style2str)
import Icons exposing (icon)

-- MODEL 

type Instrument = Piano | Bass | Drums
type alias Song = { chordProg : G.ChordProg
                  , title : String
                  , composer : String
                  , beatsPerBar : Int
                  , defaultTempo : Float
                  , style : Style
                  }
type alias Grid = List { len : Float, chord : (Maybe G.Chord, Float) }

type alias SubModel =
    { barsPerLine : Int
    , song : Song
    , playing : Bool
    , bpm : Float
    , cursor : Float
    , volPiano : Float
    , volBass : Float
    , volDrums : Float
    , playbackKey : Int
    , displayKey : Int }

init = { barsPerLine = 4
       , song = (Song G.blueBossa "Blue Bossa" "Dexter Gordon" 4 160 Styles.Bossa)
       , playing = False
       , bpm = 120
       , cursor = 0
       , volPiano = 100
       , volBass = 100
       , volDrums = 100
       , playbackKey = 0
       , displayKey = 0
       }
initCmd = Cmd.none -- R.generate DebugGenerated (JazzPiano.genRhythm 4 64) -- Cmd.none --R.generate SequenceGenerated JazzBass.bbbass

-- UPDATE

type SubMsg = 
    CaseClicked Float
    | TogglePlay
    | SetBpm Float
    | SetCursor Float
    | SeqFinished
    | SequenceGenerated Tune.Sequence
    | NextSeqGenerated Tune.Sequence
    | Edit
    | ToLibrary
    | SetVolume Instrument Float
    | Delete
    | DebugGenerated (List (Float, Float))
    | TransposePlayback Bool
    | TransposeDisplay Bool

update : SubMsg -> SubModel -> (SubModel, Cmd SubMsg)
update msg model = 
    case msg of
        CaseClicked f -> (model, Tune.setCursor f)
        TogglePlay -> ({ model | playing = not model.playing}
                      , if model.playing then Tune.pause () else Tune.play ()
                      )
        SetBpm f -> ({ model | bpm=f }, Tune.setBpm f)
        SetCursor f -> (model, Cmd.batch [ Tune.setCursor f
                                         , genSequence model ]
                       )
        SeqFinished -> ( model, genNextSeqOnly model
                                --Cmd.batch [ Tune.setCursor 0
                                --          , genSequence model ]
                       )
        SequenceGenerated b -> (model, Tune.setSequence b)

        NextSeqGenerated b -> (model, Tune.setNextSequence b)

        SetVolume i vol ->
            let 
                instStr = case i of
                    Piano -> "piano"
                    Bass -> "bass"
                    Drums -> "drums"
            in
            ( case i of
                Piano -> { model | volPiano = vol }
                Bass -> { model | volBass = vol }
                Drums -> { model | volDrums = vol }
            , Tune.setInstVolume (instStr, vol))

        DebugGenerated a -> let _ = Debug.log "debug" a in (model, Cmd.none)

        TransposePlayback isplus ->
            let 
                newmod =
                    { model | playbackKey =
                        model.playbackKey + (if isplus then 1 else -1) }
            in
               ( newmod, genSequence newmod)

        TransposeDisplay isplus -> ({ model | displayKey = model.displayKey + (if isplus then 1 else -1) }, Cmd.none)
        
        _ -> (model, Cmd.none)

seqgen : SubModel -> R.Generator Tune.Sequence
seqgen m =
    getSeqGenerator
        m.song.style
        m.bpm
        (transposeChordProg m.song.chordProg m.playbackKey)
        m.song.beatsPerBar

genSequence : SubModel -> Cmd SubMsg
genSequence m =
    let sg = seqgen m in
    Cmd.batch
        [ R.generate SequenceGenerated <| sg
        , R.generate NextSeqGenerated <| sg ]

genNextSeqOnly : SubModel -> Cmd SubMsg
genNextSeqOnly m = R.generate NextSeqGenerated <| seqgen m



view : SubModel -> Html SubMsg
view model =
    div [ class "realbook", class "content" ]
        [ p [] [ span [ class "button", onClick ToLibrary ] [ icon "menu" "Library" ] ]
        , h1 [ class "realbook", class "titleplayer" ] [ text model.song.title ]
        , div [ class "metaplayer" ]
            [ h3 [ class "realbook", class "styleplayer"  ] 
                 [ text <| (style2str model.song.style) ++ "  (" 
                    ++ (String.fromInt model.song.beatsPerBar) ++ "/4)" ]
            , h2 [ class "realbook", class "composerplayer"  ] [ text model.song.composer ]
            ]
        , div [ style "line-height" "1" ]
                [ p [ class "transposition" ] 
                       [ text <| "Playback key: " ++ (if model.playbackKey >= 0 then "+" else "") ++
                                 (String.fromInt (model.playbackKey)) ++ " semitones,"
                       , button [ onClick (TransposePlayback True) ] [ text "+"] 
                       , button [ onClick (TransposePlayback False) ] [ text "-" ] ]
                , p [ class "transposition" ] 
                       [ text <| "Display key: " ++ (if model.displayKey >= 0 then "+" else "") ++
                                 (String.fromInt (model.displayKey)) ++ " semitones (" ++
                                 (note2str <| G.pitch2note <| -model.displayKey) ++
                                 " instruments)"
                       , button [ onClick (TransposeDisplay True) ] [ text "+"] 
                       , button [ onClick (TransposeDisplay False) ] [ text "-" ] ]
                ]
        , span [ class "button", onClick TogglePlay ] 
             [ if model.playing 
               then icon "pause" "Pause"
               else icon "play_arrow" "Play" ]
        , span [ class "button", onClick (SetCursor 0) ]
                 [ icon "replay" "Start over" ]
        , input [ Html.Attributes.type_ "range"
                , onInput (\s -> (case String.toFloat s of
                                    Just f -> SetBpm f
                                    _ -> SetBpm model.bpm))
                , Html.Attributes.min "30"
                , Html.Attributes.max "300"
                , Html.Attributes.value (String.fromFloat model.bpm)
                , Html.Attributes.step "1" ] []
        , text (" " ++ String.fromFloat model.bpm ++ " BPM")
        --, p [] [ text (String.left 4 (String.fromFloat model.cursor)) ]
        , p [ style "text-align" "right" ]
            [ span [ class "button", onClick Edit ] [ icon "edit" "Edit song" ] ]
        , viewGrid model
        , div []
            [ rangeVolume Piano "piano" model.volPiano
            , rangeVolume Bass "bass" model.volBass
            , rangeVolume Drums "drums" model.volDrums ]
        , span [ class "button", onClick Delete ] [ icon "delete" "Delete song from local library" ]
        ]

rangeVolume : Instrument -> String -> Float -> Html SubMsg
rangeVolume inst str vol =
    p [] [
            input [ Html.Attributes.type_ "range"
                  , onInput (\s -> (case String.toFloat s of
                                      Just f -> SetVolume inst f
                                      _ -> SetVolume inst vol))
                  , Html.Attributes.min "0"
                  , Html.Attributes.max "100"
                  , Html.Attributes.value (String.fromFloat vol)
                  , Html.Attributes.step "1" ] []
         , text (str ++ " : " ++ (String.fromFloat vol))
         ]


{-| View a Grid. We render it as a table containing chords text representations. 
-}
viewGrid : SubModel -> Html SubMsg
viewGrid model =
    let gridlist = splitGrid (toFloat (model.barsPerLine*model.song.beatsPerBar)) <|
                        chordprog2grid model
    in
        div []
            (List.map
                (\line -> div [ class "gridline" ]
                     (List.map 
                        (\box -> div 
                            [ style "width" <| 
                                (String.fromFloat (100*box.len/(toFloat model.song.beatsPerBar)/(toFloat model.barsPerLine)))++"%"
                            --, style "text-align" "center"
                            , style "margin-top" "auto"
                            , style "display" "inline-block"
                            , style "color" 
                                    (if 
                                        model.cursor >= (Tuple.second box.chord)
                                        && model.cursor < (Tuple.second box.chord) + box.len
                                     then "red" else "black")
                            , onClick (SetCursor (Tuple.second box.chord))
                            ]
                            [ text <| chord2text (Tuple.first box.chord) ])
                        line)
                    )
                gridlist
            )

-- UTILS

substract2to2 : List number -> List number
substract2to2 l =
    case l of
        [] -> []
        h::[] -> []
        a::b::t -> (b-a)::(substract2to2 (b::t))

{--| Convert a chord progression to a grid. 
-}
chordprog2grid : SubModel -> Grid
chordprog2grid model =
    let 
        cp = transposeChordProg model.song.chordProg (model.displayKey + model.playbackKey)
    in
    List.concatMap 
        (\i -> 
            let 
                firstbeat = model.song.beatsPerBar*i
                lastbeat = model.song.beatsPerBar*(i+1)
                chordsInBar =
                    List.filter (\c -> (floor c.time) >= firstbeat && (floor c.time) < lastbeat) cp.chords |> List.sortBy .time
                maybeChords = List.map 
                    (\evt -> (Just evt.chord, evt.time)) 
                    chordsInBar
                -- If the first chord of the bar does not match the first beat, prepend a Nothing chord
                addNothing = case maybeChords of 
                    [] -> [(Nothing, toFloat firstbeat)]
                    h::t -> if (Tuple.second h) <= (toFloat firstbeat)
                                then maybeChords 
                                else (Nothing, toFloat firstbeat)::maybeChords
                times = List.map Tuple.second addNothing
                rolled = List.append times [toFloat lastbeat]
                chordsLen = substract2to2 rolled
                zip l1 l2 =
                    case (l1, l2) of
                        ([], _) -> []
                        (_, []) -> []
                        (h1::t1, h2::t2) -> {len=h1, chord=h2}::(zip t1 t2)
            in
                zip chordsLen addNothing
        )
        (List.range 0 (((floor cp.end)//model.song.beatsPerBar)-1)) -- bars indices 


{-| Transpose the chords of a progression given a number of semitones. 
-}
transposeChordProg : G.ChordProg -> Int -> G.ChordProg
transposeChordProg cp n =
    case n of
        0 -> cp
        _ -> let transnote nt = G.note2pitch 0 nt |> (+) n |> G.pitch2note in
            { cp | chords = List.map
                        (\{time, chord} ->
                            { time=time
                            , chord=
                                G.Chord
                                    (transnote chord.note)
                                    chord.type_
                                    (Maybe.map transnote chord.bass)
                            }
                        )
                        cp.chords
            }


{-| Split a grid in rows containing a given number of beats. 
-}
splitGrid : Float -> Grid -> List Grid
splitGrid beatsPerLine grid =
    let 
        recurPart done remainder i =
            if remainder == [] 
            then done 
            else
                let 
                    (before, after) = 
                        List.partition (\x -> (x.chord |> Tuple.second) < i) remainder
                in
                recurPart (before::done) after (i + beatsPerLine)
    in
        recurPart [] grid beatsPerLine |> List.reverse

{-| Convert a note to a text representation.
-}
note2str : G.Note -> String
note2str n =
    let 
        name = case n.name of
            G.C -> "C"
            G.D -> "D"
            G.E -> "E"
            G.F -> "F"
            G.G -> "G"
            G.A -> "A"
            G.B -> "B"
        alt = case n.alt of
            G.Natural -> ""
            G.Flat -> "ь"
            G.Sharp -> "#"
    in
        name++alt

{-| Convert a chord to a text representation. 
-}
chord2text : Maybe G.Chord -> String
chord2text mc =
    case mc of
        Nothing -> "%"
        Just c ->
            if c.type_ == G.NA then "NA" else
            let 
                root = note2str c.note
                typ = case c.type_ of
                    G.Dom7 -> "7"
                    G.Min7 -> "-7"
                    G.Maj7 -> "∆"
                    G.Alt7 -> "7alt"
                    G.Dom7b9 -> "7ь9"
                    G.Dom7s5 -> "7#5"
                    G.Sus4 -> "7sus4"
                    G.Min7b5 -> "-7ь5"
                    G.Dim -> "°"
                    G.MinMaj -> "-∆"
                    G.Maj7s5 -> "∆#5"
                    G.NA -> "NA"
                bass = case c.bass of
                    Nothing -> ""
                    Just n -> if n == c.note then "" else "/"++note2str n
            in
                root++typ++bass

asChordProgIn : Song -> G.ChordProg -> Song
asChordProgIn s cp = { s | chordProg=cp }
asTitleIn : Song -> String -> Song
asTitleIn s t = { s | title=t }
asDefTempoIn : Song -> Float -> Song
asDefTempoIn s t = { s | defaultTempo=t}

setTitle t s = asTitleIn s t
setComposer : String -> Song -> Song
setComposer c s = { s | composer=c}
setBeatsPerBar : Int -> Song -> Song
setBeatsPerBar i s = { s | beatsPerBar=i}
setDefTempo : Float -> Song -> Song
setDefTempo t s = { s | defaultTempo=t }
setStyle : Style -> Song -> Song
setStyle st s = { s | style=st }

asSongIn : SubModel -> Song -> SubModel
asSongIn sm s = { sm | song=s }

setBpm : Float -> SubModel -> SubModel
setBpm t sm = { sm | bpm=t }
setCursor : Float -> SubModel -> SubModel
setCursor c sm = { sm | cursor=c }
setPlaying : Bool -> SubModel -> SubModel
setPlaying b sm = { sm | playing=b }
setPlayback : Int -> SubModel -> SubModel
setPlayback i sm = { sm | playbackKey = i }
noTranspose : SubModel -> SubModel
noTranspose sm = { sm | playbackKey=0, displayKey=0 }   
module PlayerPage exposing (..)
import Generator as G
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tune
import Random as R
import Dict exposing (Dict)
import JazzDrums
import JazzBass
import JazzPiano

type alias Song = { chordProg : G.ChordProg
                  , title : String
                  , composer : String
                  , beatsPerBar : Int
                  , defaultTempo : Float
                  }
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

type alias SubModel =
    { barsPerLine : Int
    , song : Song
    , playing : Bool
    , bpm : Float
    , cursor : Float
    , volPiano : Float
    , volBass : Float
    , volDrums : Float }

asSongIn : SubModel -> Song -> SubModel
asSongIn sm s = { sm | song=s }

setBpm : Float -> SubModel -> SubModel
setBpm t sm = { sm | bpm=t }
setCursor : Float -> SubModel -> SubModel
setCursor c sm = { sm | cursor=c }

type Instrument = Piano | Bass | Drums

type SubMsg = 
    CaseClicked Float
    | TogglePlay
    | SetBpm Float
    | SetCursor Float
    | SeqFinished
    | SequenceGenerated Tune.Sequence
    | Edit
    | ToLibrary
    | SetVolume Instrument Float
    | Delete
    | DebugGenerated (List (Float, Float))
    

init = { barsPerLine = 4
       , song = (Song G.blueBossa "Blue Bossa" "Dexter Gordon" 4 160)
       , playing = False
       , bpm = 120
       , cursor = 0
       , volPiano = 100
       , volBass = 100
       , volDrums = 100
       }
initCmd = Cmd.none -- R.generate DebugGenerated (JazzPiano.genRhythm 4 64) -- Cmd.none --R.generate SequenceGenerated JazzBass.bbbass

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
        SeqFinished -> ( model, Cmd.batch [ Tune.setCursor 0
                                          , genSequence model ]
                       )
        SequenceGenerated b -> (model, Tune.setSequence b)

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
        _ -> (model, Cmd.none)

genSequence : SubModel -> Cmd SubMsg
genSequence m =
    G.mergeSeqGenerators [ JazzBass.sequenceGenerator
                         , JazzDrums.sequenceGenerator
                         , JazzPiano.sequenceGenerator ] m.song.chordProg m.song.beatsPerBar
    |> R.generate SequenceGenerated

type alias Grid = List { len : Float, chord : (Maybe G.Chord, Float) }

view : SubModel -> Html SubMsg
view model =
    div [ class "realbook" ]
        [ p [] [ button [ onClick ToLibrary ] [ text "library" ] ]
        , h1 [] [ text model.song.title ]
        , h2 [] [ text model.song.composer ]
        , button [ onClick TogglePlay ] 
                 [ text (if model.playing then "pause" else "play") ]
        , button [ onClick (SetCursor 0) ]
                 [ text "Reset" ]
        , input [ Html.Attributes.type_ "range"
                , onInput (\s -> (case String.toFloat s of
                                    Just f -> SetBpm f
                                    _ -> SetBpm model.bpm))
                , Html.Attributes.min "30"
                , Html.Attributes.max "300"
                , Html.Attributes.value (String.fromFloat model.bpm)
                , Html.Attributes.step "1" ] []
        , text ((String.fromFloat model.bpm) ++ " BPM")
        , p [] [ text (String.left 4 (String.fromFloat model.cursor)) ]
        , button [ onClick Edit ] [ text "edit song" ]
        , viewGrid model
        , div []
            [ rangeVolume Piano "piano" model.volPiano
            , rangeVolume Bass "bass" model.volBass
            , rangeVolume Drums "drums" model.volDrums ]
        , button [ onClick Delete ] [ text "Delete song from library" ]
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


substract2to2 : List number -> List number
substract2to2 l =
    case l of
        [] -> []
        h::[] -> []
        a::b::t -> (b-a)::(substract2to2 (b::t))


chordprog2grid : SubModel -> Grid
chordprog2grid model =
    let cp = model.song.chordProg in
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
                addNothing = case maybeChords of
                    [] -> [(Nothing, toFloat firstbeat)]
                    h::t -> if (Tuple.second h) <= (toFloat firstbeat)
                                then maybeChords 
                                else (Nothing, toFloat firstbeat)::maybeChords
                times = List.map Tuple.second addNothing --addNothing = good
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
        (List.range 0 (((floor cp.end)//model.song.beatsPerBar)-1))

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

viewGrid : SubModel -> Html SubMsg
viewGrid model =
    let gridlist = splitGrid (toFloat (model.barsPerLine*model.song.beatsPerBar)) <|
                        chordprog2grid model
    in
        div []
            (List.map
                (\line -> div [ style "height" "100px" ]
                     (List.map 
                        (\box -> div 
                            [ style "width" <| 
                                (String.fromInt (floor (box.len*50)))++"px"
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

chord2text : Maybe G.Chord -> String
chord2text mc =
    case mc of
        Nothing -> "%"
        Just c ->
            if c.type_ == G.NA then "NA" else
            let 
                name = case c.note.name of
                    G.C -> "C"
                    G.D -> "D"
                    G.E -> "E"
                    G.F -> "F"
                    G.G -> "G"
                    G.A -> "A"
                    G.B -> "B"
                alt = case c.note.alt of
                    G.Natural -> ""
                    G.Flat -> "β"
                    G.Sharp -> "#"
                typ = case c.type_ of
                    G.Dom7 -> "7"
                    G.Min7 -> "-7"
                    G.Maj7 -> "∆"
                    G.Alt7 -> "7alt"
                    G.Dom7b9 -> "7β9"
                    G.Dom7s5 -> "7#5"
                    G.Sus4 -> "7sus4"
                    G.Min7b5 -> "-7β5"
                    G.Dim -> "°"
                    G.MinMaj -> "-∆"
                    G.Maj7s5 -> "∆#5"
                    G.NA -> "NA"
            in
                name++alt++typ

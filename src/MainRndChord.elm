module MainRndChord exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class)
import Tune
import Generator as G
import Random as R
import JazzBass
import JazzDrums
import JazzPiano
import Styles exposing (Style)
import Icons exposing (icon)

main = Browser.element
    { init=init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
    { bpm : Float
    , cursor : Float
    , playing : Bool
    , chordProg : G.ChordProg
    , volPiano : Float
    , volBass : Float
    , volDrums : Float
    , style : Style
    }

type Msg =
    SetBpm Float
    | SeqFinished
    | SequenceGenerated Tune.Sequence
    | NextSeqGenerated Tune.Sequence
    | SetCursor Float
    | TogglePlay
    | SetVolume String Float
    | StyleChanged String

init : () -> (Model, Cmd Msg)
init _ = ({ bpm = 120
          , cursor = 0
          , playing = False
          , chordProg = G.blueBossa
          , volPiano = 100, volBass = 100, volDrums=100
          , style = Styles.Swing}
         , Cmd.batch [ genSequence Styles.Swing, genNextSequence Styles.Swing ] )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetBpm b -> ({ model | bpm=b }, Tune.setBpm b)
        TogglePlay -> ({ model | playing = not model.playing}
                      , if model.playing then Tune.pause () else Tune.play ()
                      )
        SeqFinished -> ( model, genNextSequence model.style)

        SequenceGenerated b -> (model, Tune.setSequence b)
        NextSeqGenerated b -> (model, Tune.setNextSequence b)

        SetCursor f -> ({ model | cursor = f }, Cmd.none)
        SetVolume inst vol -> 
            ( case inst of
                "piano" -> {model | volPiano = vol}
                "bass" -> {model | volBass = vol}
                _ -> {model | volDrums = vol}
            , Tune.setInstVolume (inst, vol))

        StyleChanged s ->
            let newstyle = Styles.str2style s in
            ({ model | style = newstyle }
            , Cmd.batch [ genSequence newstyle
                        , genNextSequence newstyle ])

noCrash : Tune.Sequence -> Tune.Sequence
noCrash =
    List.filter (\{instrument, pitch} -> instrument /= "drums" || pitch /= 5)

cp2seq : Style -> G.ChordProg -> R.Generator Tune.Sequence
cp2seq s cp =
    Styles.getSeqGenerator s cp 4

genSequence : Style -> Cmd Msg
genSequence s =
    fullGenerator s
    |> R.generate SequenceGenerated

genNextSequence : Style -> Cmd Msg
genNextSequence s =
    fullGenerator s
    |> R.generate NextSeqGenerated

fullGenerator : Style -> R.Generator (Tune.Sequence)
fullGenerator s =
    append_iiVI 1
    |> R.andThen (cp2seq s)
    |> R.map (noCrash)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Tune.cursorChanged SetCursor
        , Tune.sequenceFinished (always SeqFinished) ]

view : Model -> Html Msg
view model =
    div [ class "realbook", class "content", class "fulldiv"  ]
        [ h1 [ class "realbook", class "titleplayer" ] [ text "Random ii-V-Is" ]
        , select [ onInput StyleChanged ]
                 [ option [ Html.Attributes.value "Swing" ] [ text "Swing" ] 
                 , option [ Html.Attributes.value "Bossa" ] [ text "Bossa" ] 
                 , option [ Html.Attributes.value "Fusion" ] [ text "Fusion" ] 
                 ]
        , br [] []
        , span [ class "button", onClick TogglePlay ] 
             [ if model.playing 
               then icon "pause" "Pause"
               else icon "play_arrow" "Play" ]
        , input [ Html.Attributes.type_ "range"
                , onInput (\s -> (case String.toFloat s of
                                    Just f -> SetBpm f
                                    _ -> SetBpm model.bpm))
                , Html.Attributes.min "30"
                , Html.Attributes.max "300"
                , Html.Attributes.value (String.fromFloat model.bpm)
                , Html.Attributes.step "1" ] []
        , text ((String.fromFloat model.bpm) ++ " BPM")
        , rangeVolume "piano" model.volPiano
        , rangeVolume "bass" model.volBass
        , rangeVolume "drums" model.volDrums
        ]

rangeVolume : String -> Float -> Html Msg
rangeVolume inst vol =
    p [] [
            input [ Html.Attributes.type_ "range"
                  , onInput (\s -> (case String.toFloat s of
                                      Just f -> SetVolume inst f
                                      _ -> SetVolume inst vol))
                  , Html.Attributes.min "0"
                  , Html.Attributes.max "100"
                  , Html.Attributes.value (String.fromFloat vol)
                  , Html.Attributes.step "1" ] []
         , text (inst ++ " : " ++ (String.fromFloat vol))
         ]

iiVIgenerator : R.Generator G.ChordProg
iiVIgenerator =
    R.map2 
        (\i c -> 
            let
                c1 = if c==G.Min7 then G.Min7b5 else G.Min7
                c2 = if c==G.Maj7 then G.Dom7 else G.Alt7
                c3 = if c==G.Min7 then G.Min7 else G.Maj7
            in
            { chords =
                [ { time = 0, chord = { note=G.pitch2note (i+2), type_=c1, bass=Nothing }}
                , { time = 4, chord = { note=G.pitch2note (i+7), type_=c2, bass=Nothing }}
                , { time = 8, chord = { note=G.pitch2note i, type_=c3, bass=Nothing  }} ]
            , end = 16 }
        )
        (R.int 0 11) (R.uniform G.Maj7 [ G.Min7, G.Alt7 ])

append_iiVI : Int -> R.Generator G.ChordProg
append_iiVI n =
    let
        append =
            R.map2 (\cpbig cp ->
                { chords =
                    cpbig.chords ++
                    (List.map (\c -> {c | time=c.time + cpbig.end}) cp.chords)
                , end = cpbig.end + cp.end })
    in
        List.repeat (n-1) iiVIgenerator
        |> List.foldl append iiVIgenerator

module RndMelody exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes
import Tune
import Random as R
import Set exposing (Set)

main = Browser.element
    { init=init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
    { bpm : Float
    --, cusror : Float
    , playing : Bool
    , intervals : Set Int
    , length : Int
    , continuous : Bool
    , isPlayingBlank : Bool
    , blankLength : Int
    , lowend : Int
    , highend : Int
    }

type Msg =
    SetBpm Float
    | SeqFinished
    | SequenceGenerated Tune.Sequence
    | TogglePlay
    | Reset
    | Regenerate
    | SetLength Int
    | ToggleInterval Int
    | ToggleContinuous
    | SetBlankLength Int
    | AddLow Int
    | AddHigh Int

init : () -> (Model, Cmd Msg)
init _ = ({ bpm = 120
          --, cursor = 0
          , playing = False
          , intervals = Set.fromList [1,2]
          , length = 8
          , continuous = False
          , isPlayingBlank = False
          , blankLength = 8
          , lowend = 48
          , highend = 59 }
         , genSequence (Set.fromList [1,2]) 8 48 59)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetBpm b -> ({ model | bpm=b }, Tune.setBpm b)
        SeqFinished -> 
            if model.continuous
            then
                ({ model | isPlayingBlank = not model.isPlayingBlank }
                , if model.isPlayingBlank
                  then genSequence model.intervals model.length model.lowend model.highend
                  else Tune.setSequenceAndPlay [ Tune.Event (toFloat (model.blankLength)) False 0 "piano" 0 ]
                )
            else
                ({ model | playing = False }
                , Cmd.batch [ Tune.setCursor 0, Tune.pause () ])

        TogglePlay -> ({ model | playing = not model.playing}
                      , if model.playing 
                        then 
                            Cmd.batch [ Tune.pause (), Tune.setCursor 0 ]
                        else Tune.play ()
                      )
        SequenceGenerated b -> 
            ({ model | isPlayingBlank = False }
            --, Tune.setSequenceAndPlay b)
            , if model.playing then Tune.setSequenceAndPlay b else Tune.setSequence b)

        Reset -> (model, Tune.setCursor 0) 

        Regenerate -> ( { model | playing = True }
                      , genSequence model.intervals model.length model.lowend model.highend)

        SetLength l -> ({ model | length=l }, Cmd.none)

        ToggleInterval i -> ({ model | intervals=toggleSet i model.intervals }, Cmd.none)

        ToggleContinuous -> ({ model | continuous = not model.continuous }, Cmd.none)

        SetBlankLength l -> ({ model | blankLength = l }, Cmd.none)

        AddLow n -> (if model.lowend + n <= model.highend then { model | lowend = model.lowend + n } else model, Cmd.none)

        AddHigh n -> (if model.lowend <= model.highend + n then { model | highend = model.highend + n } else model, Cmd.none)

toggleSet : comparable -> Set comparable -> Set comparable
toggleSet c s =
    if Set.member c s then Set.remove c s else Set.insert c s

genSequence : Set Int -> Int -> Int -> Int -> Cmd Msg --R.Generator Tune.Sequence
genSequence intvls len low high = 
    Set.toList intvls
    |> (\l -> case l of
                [] -> R.constant 0
                h::t -> R.uniform h t)
    |> R.list (len-1)
    |> R.map2
        (List.map2
            (\it x -> it*x))
        (R.list (len-1) (R.map (\t -> 2*t-1) (R.int 0 1)))
    |> R.andThen
        (\list -> R.int low high
            |> R.map
            (\start ->
                List.foldl
                    (\intvl cuml -> 
                        case cuml of
                            [] -> []
                            h::t -> (h+intvl)::cuml
                    )
                    [start]
                    list
                |> List.reverse
            )
        )
    |> R.map
        (List.indexedMap
            (\id_ pitch ->
                let id = toFloat id_ in
                [Tune.Event id True pitch "piano" 1,
                 Tune.Event (id+0.9) False pitch "piano" 1 ])
         >> List.concat)
    |> R.generate SequenceGenerated

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick TogglePlay ] [ text (if model.playing then "Pause" else "Replay") ]
        , if not model.playing then button [ onClick Regenerate ] [ text "Play new" ] else text ""
        , br [] []
        , button [ Html.Attributes.style "background-color" (if model.continuous then "red" else "white")
                 , onClick ToggleContinuous ] [ text "Continuous generation" ]
        , if model.continuous
          then 
              p [] 
                [ text "Blank duration: "
                , input [ Html.Attributes.type_ "number" 
                        , Html.Attributes.min "0"
                        , onInput (\s -> SetBlankLength
                           (case String.toInt s of
                               Just i -> i
                               _ -> 0))
                        , Html.Attributes.value <| String.fromInt model.blankLength
                        ] [] 
                ]
          else text ""
        , br [] [], br [] []
        , input [ Html.Attributes.type_ "range"
                , onInput (\s -> (case String.toFloat s of
                                    Just f -> SetBpm f
                                    _ -> SetBpm model.bpm))
                , Html.Attributes.min "30"
                , Html.Attributes.max "300"
                , Html.Attributes.value (String.fromFloat model.bpm)
                , Html.Attributes.step "1" ] []
        , text ((String.fromFloat model.bpm) ++ " BPM")
        , br [] []
        , text "Length: "
        , input [ Html.Attributes.type_ "number" 
                , Html.Attributes.min "0"
                , onInput (\s -> SetLength
                   (case String.toInt s of
                       Just i -> i
                       _ -> 0))
                , Html.Attributes.value <| String.fromInt model.length
                ] []
        , p []
            [ togBtn "unison" (Set.member 0 model.intervals) (ToggleInterval 0)
            , togBtn "2nd m" (Set.member 1 model.intervals) (ToggleInterval 1)
            , togBtn "2nd M" (Set.member 2 model.intervals) (ToggleInterval 2)
            , togBtn "3rd m" (Set.member 3 model.intervals) (ToggleInterval 3)
            , togBtn "3rd M" (Set.member 4 model.intervals) (ToggleInterval 4)
            , togBtn "4th" (Set.member 5 model.intervals) (ToggleInterval 5)
            , togBtn "tritone" (Set.member 6 model.intervals) (ToggleInterval 6)
            , togBtn "5th" (Set.member 7 model.intervals) (ToggleInterval 7)
            , togBtn "6th m" (Set.member 8 model.intervals) (ToggleInterval 8)
            , togBtn "6th M" (Set.member 9 model.intervals) (ToggleInterval 9)
            , togBtn "7th m" (Set.member 10 model.intervals) (ToggleInterval 10)
            , togBtn "7th M" (Set.member 11 model.intervals) (ToggleInterval 11)
            , togBtn "8th" (Set.member 12 model.intervals) (ToggleInterval 12)
            ]
        , p [] 
            [ text ("Starting note range : " ++ pitch2note model.lowend)
            , button [ onClick (AddLow -1) ] [ text "-" ]
            , button [ onClick (AddLow 1) ] [ text "+" ]
            , text (" to " ++ pitch2note model.highend)
            , button [ onClick (AddHigh -1) ] [ text "-" ]
            , button [ onClick (AddHigh 1) ] [ text "+" ]
            ]
        ]

togBtn : String -> Bool -> msg -> Html msg
togBtn t b m =
    button [ Html.Attributes.style "background-color" (if b then "red" else "white")
           , onClick m ]
           [ text t ]


subscriptions : Model -> Sub Msg
subscriptions model =
        Tune.sequenceFinished (always SeqFinished)

pitch2note n = 
    let
        name = 
            case modBy 12 n of
                0 -> "C "
                1 -> "C#"
                2 -> "D "
                3 -> "Eb"
                4 -> "E "
                5 -> "F "
                6 -> "F#"
                7 -> "G "
                8 -> "Ab"
                9 -> "A "
                10 -> "Bb"
                _ -> "B "
        oct = String.fromInt (n//12 - 1)
    in 
        name ++ oct

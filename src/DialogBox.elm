--import Main
module DialogBox exposing (..)
import Html exposing (..)
import Html.Events as E
import Html.Attributes exposing (..)

type alias DialogBox mdl msg = 
    { title: String
    , body: mdl -> Html msg --Main.Msg
    , actions: List { text: String, cmd: msg }
    --, mapping: MsgDb -> msg
    }

--yesNoDialog : (MsgDb -> msg) -> String -> String -> msg -> msg -> DialogBox mdl msg
--yesNoDialog mapping title subtext yescmd nocmd =
yesNoDialog : String -> String -> msg -> msg -> DialogBox mdl msg
yesNoDialog title subtext yescmd nocmd =
    DialogBox 
        title
        (always <| text subtext)
        [ {text="Yes", cmd=yescmd}, {text="No", cmd=nocmd} ]
        --mapping
        

displayDialog : mdl -> DialogBox mdl msg -> Html msg
displayDialog mdldb dialogBox =
    div [ class "w3-modal" , style "display" "block" ]
        [ div [ class "w3-modal-content" ]
            [ div [ class "w3-container", style "padding-bottom" "15px" ]
                [ h1 [] [ text dialogBox.title ]
                , dialogBox.body mdldb
                , div []
                    (List.map 
                        (\x -> button [ E.onClick x.cmd ] [ text x.text ] )
                        dialogBox.actions)
                ]
            ]
        ]


type alias ModelDB =
    { beatsPerBar: Int
    , nBars: Int
    , title: String
    , composer: String
    }

initDb : ModelDB
initDb = { beatsPerBar=4, nBars=12, title="New song", composer="Unknown" }

type MsgDb =
    SetBeatsPerBar Int
    | SetNBars Int
    | SetTitle String
    | SetComposer String

updateDb : MsgDb -> ModelDB -> ModelDB
updateDb msg model =
    case msg of
        SetBeatsPerBar n -> { model | beatsPerBar=n }
        SetNBars n -> { model | nBars=n }
        SetTitle s -> { model | title=s }
        SetComposer s -> { model | composer=s }

newSongDialog : (MsgDb -> msg) -> msg -> msg -> DialogBox ModelDB msg 
newSongDialog mapping yesmsg nomsg =
    DialogBox
        "New Song"
        (\mdl -> 
            Html.map mapping
               (div []
                    [ h4 [] [ text "Set time signature and meta-information for your new song" ]
                    , div [] [ text "Time signature :", signatureSelectBar mdl.beatsPerBar ]
                    , div [] [ text "#Bars: "
                             , input [ type_ "number" 
                                     , Html.Attributes.min "0"
                                     , E.onInput (\s -> SetNBars
                                        (case String.toInt s of
                                            Just i -> i
                                            _ -> 0))
                                     , value <| String.fromInt mdl.nBars
                                     ] []
                             ]
                    , div [] [ text "Title: ", input [ type_ "text", value mdl.title, E.onInput SetTitle ] [] ]
                    , div [] [ text "Composer: ", input [ type_ "text", value mdl.composer, E.onInput SetComposer ] [] ]
                    ]
               ) 
        )
        [ { text="Create Song", cmd=yesmsg }, { text="Cancel", cmd=nomsg } ]

signatureSelectBar : Int -> Html MsgDb
signatureSelectBar sig =
    let
        buttonAttr t = [ style "background-color" (if sig/=t then "#e7e7e7" else "#f44336")
                       , E.onClick (SetBeatsPerBar t) ]
    in
    div [ style "color" "white" ]
        [ button (buttonAttr 3) [ text "3/4" ]
        , button (buttonAttr 4) [ text "4/4" ]
        , button (buttonAttr 5) [ text "5/4" ]
        ]


--type alias DialogBoxModel model msg =
--    { title: String
--    , body: Html msg
--    , actions: List { text: String, mod: model, cmd: Cmd msg }
--    }
--
--displayDialogModel : DialogBoxModel model msg -> (model -> Cmd msg -> msg) -> Html msg
--displayDialogModel db2 action =
--    div [ class "w3-modal" , style "display" "block" ]
--        [ div [ class "w3-modal-content" ]
--            [ div [ class "w3-container" ]
--                [ h1 [] [ text db2.title ]
--                , db2.body
--                , div []
--                    (List.map 
--                        (\x -> button [ E.onClick (action x.mod x.cmd) ] [ text x.text ] )
--                        db2.actions)
--                ]
--            ]
--        ]

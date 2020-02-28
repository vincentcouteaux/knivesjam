--import Main
module DialogBox exposing (..)
import Html exposing (..)
import Html.Events as E
import Html.Attributes exposing (..)

type alias DialogBox msg = 
    { title: String
    , body: Html msg --Main.Msg
    , actions: List { text: String, cmd: msg }
    }

yesNoDialog : String -> String -> msg -> msg -> DialogBox msg
yesNoDialog title subtext yescmd nocmd =
    DialogBox 
        title
        (text subtext)
        [ {text="Yes", cmd=yescmd}, {text="No", cmd=nocmd} ]

displayDialog : DialogBox msg -> Html msg
displayDialog dialogBox =
    div [ class "w3-modal" , style "display" "block" ]
        [ div [ class "w3-modal-content" ]
            [ div [ class "w3-container" ]
                [ h1 [] [ text dialogBox.title ]
                , dialogBox.body
                , div []
                    (List.map 
                        (\x -> button [ E.onClick x.cmd ] [ text x.text ] )
                        dialogBox.actions)
                ]
            ]
        ]

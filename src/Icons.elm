module Icons exposing (..)
import Html
import Html.Attributes

icon : String -> String -> Html.Html msg
icon s hover = Html.i [ Html.Attributes.class "material-icons"
                      , Html.Attributes.title hover ] [ Html.text s ]

module Icons exposing (..)
import Html
import Html.Attributes

icon : String -> Html.Html msg
icon s = Html.i [ Html.Attributes.class "material-icons" ] [ Html.text s ]

module Library exposing (..)
import PlayerPage as Pp
import Generator as G
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)

type alias SubModel = Dict String Pp.Song

type SubMsg = SongClicked Pp.Song | NewSong | Close

init : SubModel
init = Dict.fromList [("Blue Bossa--Dexter Gordon", Pp.Song G.blueBossa "Blue Bossa" "Dexter Gordon" 4)]

update : SubMsg -> SubModel -> (SubModel, Cmd SubMsg)
update _ m = (m, Cmd.none)

view : SubModel -> Html SubMsg
view m =
    div [] <|
        (button [ onClick Close ] [ text "Close" ])::(
            List.map 
                 (\s -> div [ onClick (SongClicked s) ]
                            [ text <| s.title ++ " - " ++ s.composer ])
                 (Dict.values m)
        ) ++ [div [ onClick NewSong ] [ a [] [ text "New Song" ] ]]

addSong : SubModel -> Pp.Song -> SubModel
addSong m s =
    let
        key = (escape s.title) ++ "--" ++ (escape s.composer)
    in
        Dict.insert key s m

escape : String -> String
escape s = String.foldr 
            (\c t -> if c == '-' then "\\-"++t else (String.cons c t))
            ""
            s
    

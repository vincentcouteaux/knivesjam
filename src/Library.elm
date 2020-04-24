port module Library exposing (..)

import PlayerPage as Pp
import Generator as G
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Json.Encode as Je
import Json.Decode as Jd
import Icons exposing (icon)
import Styles exposing (..)
import File.Download as Dl
import File.Select as Select
import File exposing (File)
import Task
import DialogBox exposing (..)

type alias SubModel = { library: Dict String Pp.Song
                      , searchBar: String
                      , hasChosen: Bool
                      , dialogBox: Maybe (DialogBox () SubMsg) }

type SubMsg = 
    SongClicked Pp.Song 
    | NewSong 
    | Close
    | GotASong String
    | Delete Pp.Song
    | SearchBarChanged String
    | DownloadJson
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | CancelDb
    | AddFile2LocalDb (List Pp.Song)

init : SubModel
init = { library=Dict.empty
       , searchBar=""
       , hasChosen=False
       , dialogBox=Nothing }--Dict.fromList [("Blue Bossa--Dexter Gordon", Pp.Song G.blueBossa "Blue Bossa" "Dexter Gordon" 4)]

initCmd : Cmd msg
initCmd = queryAllSongs ()

update : SubMsg -> SubModel -> (SubModel, Cmd SubMsg)
update msg m = 
    case msg of
        GotASong s -> 
            let
                resultSong = Jd.decodeString json2song s
            in
                case resultSong of
                    Ok song -> (addSong m song, Cmd.none)
                    Err e -> let _ = Debug.log "decding: " (Jd.errorToString e) in (m, Cmd.none)

        Delete s -> let key = getKey s in (remSong m key, deleteSong key)

        SearchBarChanged s -> ({ m | searchBar = s }, Cmd.none)

        DownloadJson -> (m, Dl.string "knivesjam_library.json" "text/json"
                            <| Je.encode 2
                            <| Je.list song2json (Dict.values m.library))

        JsonRequested -> (m, Select.file ["text/json"] JsonSelected)

        JsonSelected file -> (m, Task.perform JsonLoaded (File.toString file))

        JsonLoaded str -> 
            case Jd.decodeString (Jd.list json2song) str of
                Err _ -> ({ m | dialogBox = Just <|
                    DialogBox 
                            "Error"
                            (always (text "Cannot read file"))
                            [ { text="OK", cmd=CancelDb } ] }, Cmd.none)
                Ok res -> ({ m | dialogBox = Just <|
                    DialogBox
                        "Load database"
                        (always (text ("Do you want to add " ++ (String.fromInt (List.length res)) ++ " songs to your Database ?")))
                        [ { text="Yes", cmd=AddFile2LocalDb res}
                        , { text="No", cmd=CancelDb } ] }, Cmd.none)
        CancelDb -> ({m | dialogBox = Nothing }, Cmd.none)

        AddFile2LocalDb res ->
            let
                library = 
                    List.foldl
                        (\s dic -> Dict.insert (getKey s) s dic)
                        m.library
                        res
            in
                ({ m | library = library, dialogBox = Nothing }
                , Cmd.batch
                <| List.map addSong2db
                <| List.map song2json
                <| res )
            
        _ ->(m, Cmd.none)

view : SubModel -> Html SubMsg
view m =
    div []
        ((if m.hasChosen then div [ class "xbutton", onClick Close ] [ icon "close" "Close" ] else text "")::
        (
            case m.dialogBox of
                Nothing -> []
                Just db -> [ displayDialog () db ]
        )
        ++
        [ div [ style "text-align" "center"
              , style "margin-bottom" "15px"
              , style "margin-top" "15px" ]
            [ img [ src "knivesjam_logo.svg", alt "logo", Html.Attributes.width 200 ] [] ]
        , div [ style "text-align" "center" ] [ input [ type_ "text"
                                                      , placeholder "search"
                                                      , value m.searchBar
                                                      , onInput SearchBarChanged ] [] ]
        , div [ class "toprow", style "height" "50px" ]
              [ div [ class "left"
                    , class "newsong"
                    , onClick NewSong ] [ text "Add new song" ]
              , div [ class "right", style "padding" "15px" ] 
                    [ span [ onClick DownloadJson
                           , style "margin" "5px"
                           , style "cursor" "pointer" ] [ icon "cloud_download" "Save library" ]
                    , span [ onClick JsonRequested
                           , style "marging" "5px"
                           , style "cursor" "pointer" ] [ icon "cloud_upload" "Load a library file" ]
                    ]
              ]
        , div [ class "songlist" ]
            (Dict.values m.library
            |> List.filter
                (\s -> contains m.searchBar s.title || contains m.searchBar s.composer)
            |> List.map 
                 (\s -> div [ class "libsongcontainer", onClick (SongClicked s) ]
                            [ div [ class "left" ] [ text s.title ], div [ class "right" ] [ text s.composer ] ])
            )
        ])

getKey : Pp.Song -> String
getKey s = (escape s.title) ++ "--" ++ (escape s.composer)

addSong : SubModel -> Pp.Song -> SubModel
addSong m s =
    { m | library=Dict.insert (getKey s) s m.library }
remSong : SubModel -> String -> SubModel
remSong m k =
    { m | library=Dict.remove k m.library }

setChosen : SubModel -> SubModel
setChosen m = { m | hasChosen = True }

escape : String -> String
escape s = String.foldr 
            (\c t -> if c == '-' then "\\-"++t else (String.cons c t))
            ""
            s

contains : String -> String -> Bool
contains s1 s2 =
    String.contains (String.toLower s1) (String.toLower s2)
    
type Style = Bop | Bossa

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
            G.Natural -> "N"
            G.Flat -> "F"
            G.Sharp -> "S"
    in
        name ++ alt

str2note : String -> G.Note
str2note s =
    let
        name = case (String.left 1 s) of
            "C" -> G.C
            "D" -> G.D
            "E" -> G.E
            "F" -> G.F
            "G" -> G.G
            "A" -> G.A
            _ -> G.B
        alt = case (String.right 1 s) of
            "N" -> G.Natural
            "F" -> G.Flat
            _ -> G.Sharp
    in
        G.Note name alt

str2mnote : String -> Maybe G.Note
str2mnote s =
    case s of
        "-" -> Nothing
        _ -> Just (str2note s)


type2str : G.ChordType -> String
type2str ct =
    case ct of
        G.Dom7 -> "Dom7"
        G.Min7 -> "-7"
        G.Maj7 -> "Maj7"
        G.Alt7 -> "7alt"
        G.Sus4 -> "7sus4"
        G.Dom7b9 -> "7b9"
        G.Dom7s5 -> "7s5"
        G.Min7b5 -> "-7b5"
        G.Dim -> "dim"
        G.MinMaj -> "minmaj"
        G.Maj7s5 -> "maj7s5"
        G.NA -> "NA"
str2type : String -> G.ChordType
str2type ct =
    case ct of
        "Dom7" -> G.Dom7
        "-7" -> G.Min7
        "Maj7" -> G.Maj7
        "7alt" -> G.Alt7
        "7sus4" -> G.Sus4
        "7b9" -> G.Dom7b9
        "7s5" -> G.Dom7s5
        "-7b5" -> G.Min7b5
        "dim" -> G.Dim
        "minmaj" -> G.MinMaj
        "maj7s5" -> G.Maj7s5
        _ -> G.NA

song2json : Pp.Song -> Je.Value
song2json song =
    Je.object
        [ ("key", Je.string (getKey song))
        , ("title", Je.string song.title)
        , ("composer", Je.string song.composer)
        , ("beatsPerBar", Je.int song.beatsPerBar)
        , ("defaultTempo", Je.float song.defaultTempo)
        , ("style", Je.string (style2str song.style))
        , ("chordProg",
            Je.object
                [ ("chords",
                    Je.list
                        (\{time, chord} ->
                            Je.object
                                [ ("time", Je.float time)
                                , ("chord",
                                    Je.object
                                        [ ("note", Je.string (note2str chord.note))
                                        , ("type_", Je.string (type2str chord.type_))
                                        , ("bass", Je.string <|
                                            case chord.bass of
                                                Nothing -> "-"
                                                Just s -> note2str s
                                          )
                                        ]
                                  )
                                  ]
                        )
                        song.chordProg.chords
                    )
                  , ("end", Je.float song.chordProg.end) ]
            ) ]

flatten : Maybe (Maybe a) -> Maybe a
flatten = Maybe.andThen identity

json2song : Jd.Decoder Pp.Song
json2song =
    Jd.map6 Pp.Song
        (Jd.field "chordProg"
            (Jd.map2 G.ChordProg
                (Jd.field "chords"
                    (Jd.list
                        (Jd.map2 (\t c -> { time=t, chord=c })
                            (Jd.field "time" Jd.float)
                            (Jd.field "chord"
                                (Jd.map3 G.Chord
                                    (Jd.field "note" 
                                        (Jd.map str2note Jd.string)
                                    )
                                    (Jd.field "type_"
                                        (Jd.map str2type Jd.string)
                                    )
                                    (Jd.map flatten 
                                        (Jd.maybe
                                            (Jd.field "bass"
                                                (Jd.map str2mnote Jd.string)
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
                (Jd.field "end" Jd.float)
            )
        )
        (Jd.field "title" Jd.string)
        (Jd.field "composer" Jd.string)
        (Jd.field "beatsPerBar" Jd.int)
        (Jd.field "defaultTempo" Jd.float)
        (Jd.field "style" (Jd.map str2style Jd.string))


port addSong2db : Je.Value -> Cmd msg

port queryAllSongs : () -> Cmd msg

port gotASong : (String -> msg) -> Sub msg

port deleteSong : String -> Cmd msg

       -- { key : String
       --           , title : String
       --           , composer : String
       --           , beatsPerBar : Int
       --           , defaultTempo : Float
       --           , style : Style
       --           , chordProg : G.ChordProg } -> Cmd msg






module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes
import Tune
import Generator as G
import Random as R
import PlayerPage as Pp
import Editor as E
import Library as L
import DialogBox exposing (..)
import Dict exposing (Dict)

main = Browser.element
    { init=init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Page = Player | Editor | Library

type alias Model =
    { playerModel : Pp.SubModel
    , editorModel : E.SubModel
    , libraryModel : L.SubModel
    , dialogBoxModel : ModelDB
    , curPage : Page
    , dialogBox : Maybe (DialogBox ModelDB Msg)
    --, dialogBox2 : Maybe (DialogBoxModel Model Msg)
    }

asPlayerModIn : Model -> Pp.SubModel -> Model
asPlayerModIn mod ppmod = { mod | playerModel = ppmod }
asEditModIn : Model -> Pp.SubModel -> Model
asEditModIn mod ppmod = { mod | playerModel = ppmod }
setLibrary : L.SubModel -> Model -> Model
setLibrary l m = { m | libraryModel = l }
asDialogBoxIn : Model -> DialogBox ModelDB Msg -> Model
asDialogBoxIn m d = { m | dialogBox = Just d }
setDbModel : ModelDB -> Model -> Model
setDbModel mdldb m = { m | dialogBoxModel = mdldb }
--asDialogBox2In : Model -> DialogBoxModel Msg -> Model
--asDialogBoxIn m d = { m | dialogBox2 = Just d }
resetDialog : Model -> Model
resetDialog m = { m | dialogBox = Nothing }

type Msg =
    SetCursor Float
    | PpEvent Pp.SubMsg
    | EditorEvent E.SubMsg
    | LibEvent L.SubMsg
    | DialogEvent MsgDb
    | ChangePage Page
    | CreateSong
    | ResetDialog
    --| DialogResult Model (Cmd Msg)

init : () -> (Model, Cmd Msg)
init _ = ({ playerModel = Pp.init
          , editorModel = E.init
          , libraryModel = L.init
          , curPage = Library
          , dialogBox = Nothing
          , dialogBoxModel = initDb }
         , Cmd.batch
            [ Cmd.map (\sm -> PpEvent sm) Pp.initCmd
            , L.initCmd ])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetCursor f -> let submod = model.playerModel in
                        ({ model | playerModel={ submod | cursor=f}}
                        , Cmd.none)
        PpEvent submsg -> 
            case submsg of
                Pp.Edit ->
                    let
                        --oldEditorModel = model.editorModel
                        einit = E.init
                        newEditorModel = 
                            { einit | 
                              grid = E.chordprog2grid model.playerModel
                            , title=model.playerModel.song.title
                            , composer=model.playerModel.song.composer
                            , beatsPerBar=model.playerModel.song.beatsPerBar
                            , defTempo=model.playerModel.song.defaultTempo }
                    in
                    ({ model | curPage = Editor, editorModel = newEditorModel }
                     , Cmd.none) 

                Pp.ToLibrary -> ({ model | curPage = Library }, Cmd.none)

                Pp.Delete ->
                    (
                        yesNoDialog
                            "Delete"
                            ("Are you sure you want to delete \"" ++ model.playerModel.song.title ++ "\" by " ++ model.playerModel.song.composer ++ " from your local Library ? It cannot be undone")
                            (LibEvent <| L.Delete model.playerModel.song)
                            ResetDialog
                        |> asDialogBoxIn model
                    , Cmd.none)

                _ ->
                    let 
                        (newmod, newcmd) = Pp.update submsg model.playerModel
                    in
                        ({ model | playerModel = newmod}
                        , Cmd.map (\sm -> PpEvent sm) newcmd )

        EditorEvent submsg -> 
            case submsg of
                E.Quit -> 
                    (
                        yesNoDialog 
                            "Sure ?"
                            "it will not be saved" 
                            (EditorEvent E.ConfirmQuit)
                            ResetDialog
                        |> asDialogBoxIn model
                    , Cmd.none)
                E.ConfirmQuit -> ({ model | curPage = Player, dialogBox = Nothing}, Cmd.none)
                E.SaveAndQuit ->
                    let
                        newsong =
                            E.grid2chordprog model.editorModel.grid
                                |> Pp.asChordProgIn model.playerModel.song
                                |> Pp.setTitle model.editorModel.title
                                |> Pp.setComposer model.editorModel.composer
                                |> Pp.setBeatsPerBar model.editorModel.beatsPerBar
                                |> Pp.setDefTempo model.editorModel.defTempo
                        newmod =
                            newsong
                                |> Pp.asSongIn model.playerModel
                                |> Pp.setBpm model.editorModel.defTempo
                                |> asPlayerModIn model
                                |> setLibrary (L.addSong model.libraryModel newsong)
                    in
                    ( { newmod | curPage = Player }
                    , Cmd.batch
                        [ genSequence newmod
                        , L.addSong2db (L.song2json newsong)
                        , Tune.setBpm model.editorModel.defTempo ]
                    ) 
                _ ->
                    let 
                        (newmod, newcmd) = E.update submsg model.editorModel
                    in
                        ({ model | editorModel = newmod}
                        , Cmd.map (\sm -> EditorEvent sm) newcmd )
        
        LibEvent submsg ->
            case submsg of
                L.SongClicked s ->
                    let
                        newmod =
                            s |> Pp.asSongIn model.playerModel
                              |> Pp.setBpm s.defaultTempo
                              |> Pp.setCursor 0
                              |> asPlayerModIn model
                    in
                        ( { newmod | curPage = Player }
                        , Cmd.batch
                            [ genSequence newmod
                            , Tune.setCursor 0
                            , Tune.setBpm s.defaultTempo ] )
                L.Close ->
                    ({ model | curPage = Player }, genSequence model)

                L.NewSong ->
                    ( newSongDialog DialogEvent CreateSong ResetDialog
                      |> asDialogBoxIn model
                      |> setDbModel initDb
                    , Cmd.none )
                _ ->
                    let 
                        (newmod, newcmd) = L.update submsg model.libraryModel
                        mm = case submsg of
                            L.Delete _ -> { model | curPage = Library, dialogBox = Nothing}
                            _ -> model
                    in
                        ({ mm | libraryModel = newmod }
                        , Cmd.map (\sm -> LibEvent sm) newcmd )

        ChangePage newp -> ({ model | curPage = newp }, Cmd.none)

        CreateSong ->
            ({ model | editorModel=E.initwith
                model.dialogBoxModel.beatsPerBar
                model.dialogBoxModel.nBars
                model.dialogBoxModel.title
                model.dialogBoxModel.composer
             , curPage=Editor
             , dialogBox=Nothing }, Cmd.none)

        DialogEvent msgdb -> ({ model | dialogBoxModel = updateDb msgdb model.dialogBoxModel }, Cmd.none)

        ResetDialog -> (resetDialog model, Cmd.none)

        --DialogResult m c -> (m, c)

genSequence : Model -> Cmd Msg
genSequence m = Cmd.map (\sm -> PpEvent sm) (Pp.genSequence m.playerModel)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Tune.cursorChanged SetCursor
        , Tune.sequenceFinished (always (PpEvent Pp.SeqFinished))
        , L.gotASong (LibEvent << L.GotASong) ]

view : Model -> Html Msg
view model =
    div [] <|
        (
            case model.dialogBox of
                Nothing -> []
                Just db -> [ displayDialog model.dialogBoxModel db ]
        )
        ++
        [ 
            case model.curPage of
                Player ->
                    Html.map (\submsg -> PpEvent submsg) (Pp.view model.playerModel)
                Editor ->
                    Html.map (\submsg -> EditorEvent submsg) (E.view model.editorModel)
                Library ->
                    Html.map (\submsg -> LibEvent submsg) (L.view model.libraryModel)
        ]


-- UTILS

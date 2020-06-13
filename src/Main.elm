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
import MainRndChord as Rnc
import DialogBox exposing (..)
import Dict exposing (Dict)

main = Browser.element
    { init=init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Page = Player | Editor | Library | RndChord

type alias Model =
    { playerModel : Pp.SubModel
    , editorModel : E.SubModel
    , libraryModel : L.SubModel
    , dialogBoxModel : ModelDB
    , rndChordModel : Rnc.Model
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
    | RncEvent Rnc.Msg
    | DialogEvent MsgDb
    | ChangePage Page
    | CreateSong
    | ResetDialog
    --| DialogResult Model (Cmd Msg)

init : () -> (Model, Cmd Msg)
init _ = 
    let (rncMod, rncCmd) = Rnc.init () in
    ({ playerModel = Pp.init
     , editorModel = E.init
     , libraryModel = L.init
     , rndChordModel = rncMod
     , curPage = Library
     , dialogBox = Nothing
     , dialogBoxModel = initDb }
    , Cmd.batch
    [ Cmd.map (\sm -> PpEvent sm) Pp.initCmd
    , L.initCmd ])
    --, Cmd.map (\sm -> RncEvent sm) rncCmd ])

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
                              grid = E.chordprog2grid (Pp.noTranspose model.playerModel)
                            , title=model.playerModel.song.title
                            , composer=model.playerModel.song.composer
                            , beatsPerBar=model.playerModel.song.beatsPerBar
                            , defTempo=model.playerModel.song.defaultTempo
                            , style=model.playerModel.song.style }
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
                            "Quit editor ?"
                            "All unsaved modifications will be lost" 
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
                                |> Pp.setStyle model.editorModel.style
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
                              |> Pp.setPlaying False
                              |> asPlayerModIn model
                              |> setLibrary (L.setChosen model.libraryModel)
                    in
                        ( { newmod | curPage = Player }
                        , Cmd.batch
                            [ genSequence newmod
                            , Tune.setCursor 0
                            , Tune.setBpm s.defaultTempo
                            , Tune.pause () ] )
                L.ToRndChord ->
                    let (rncMod, rncCmd) = Rnc.init () in
                    ( { model | curPage = RndChord, rndChordModel = rncMod }
                    , Cmd.map (\sm -> RncEvent sm) 
                    <| Cmd.batch
                        [ Tune.pause ()
                        , Tune.setCursor 0
                        , Tune.setBpm rncMod.bpm
                        , rncCmd ] )
                L.Close ->
                    ({ model | curPage = Player, libraryModel = L.setChosen model.libraryModel }
                    , genSequence model)

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

        RncEvent submsg ->
            case submsg of
                Rnc.ToMenu -> ({ model | curPage = Library }, Cmd.none)
                _ -> 
                    let (rncMod, rncCmd) = Rnc.update submsg model.rndChordModel in
                    ({ model | rndChordModel = rncMod }
                    , Cmd.map (\sm -> RncEvent sm) rncCmd)

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
        , L.gotASong (LibEvent << L.GotASong)
        , if model.curPage /= RndChord
          then Tune.sequenceFinished (always (PpEvent Pp.SeqFinished))
          else Sub.map (\sm -> RncEvent sm) (Rnc.subscriptions model.rndChordModel) ]

view : Model -> Html Msg
view model =
    div [ Html.Attributes.classList [("fulldiv", True), ("coffee", model.curPage == Library) ] ] <|
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

                RndChord ->
                    Html.map (\sm -> RncEvent sm) (Rnc.view model.rndChordModel)
        ]


-- UTILS

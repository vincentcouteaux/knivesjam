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
    , curPage : Page
    , dialogBox : Maybe (DialogBox Msg)
    }

asPlayerModIn : Model -> Pp.SubModel -> Model
asPlayerModIn mod ppmod = { mod | playerModel = ppmod }
asEditModIn : Model -> Pp.SubModel -> Model
asEditModIn mod ppmod = { mod | playerModel = ppmod }
setLibrary : L.SubModel -> Model -> Model
setLibrary l m = { m | libraryModel = l }
asDialogBoxIn : Model -> DialogBox Msg -> Model
asDialogBoxIn m d = { m | dialogBox = Just d }
resetDialog : Model -> Model
resetDialog m = { m | dialogBox = Nothing }

type Msg =
    SetCursor Float
    | PpEvent Pp.SubMsg
    | EditorEvent E.SubMsg
    | LibEvent L.SubMsg
    | ChangePage Page
    | ResetDialog

init : () -> (Model, Cmd Msg)
init _ = ({ playerModel = Pp.init
          , editorModel = E.init
          , libraryModel = L.init
          , curPage = Library
          , dialogBox = Nothing }
         , Cmd.map (\sm -> PpEvent sm) Pp.initCmd)

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
                        newEditorModel = { einit | grid = E.chordprog2grid model.playerModel, title=model.playerModel.song.title, composer=model.playerModel.song.composer }
                    in
                    ({ model | curPage = Editor, editorModel = newEditorModel }
                     , Cmd.none) 

                Pp.ToLibrary -> ({ model | curPage = Library }, Cmd.none)

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
                        newmod =
                            newsong
                                |> Pp.asSongIn model.playerModel
                                |> asPlayerModIn model
                                |> setLibrary (L.addSong model.libraryModel newsong)
                    in
                    ({ newmod | curPage = Player }, Cmd.map (\sm -> PpEvent sm) (Pp.genBassLine newmod.playerModel)) 
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
                              |> asPlayerModIn model
                    in
                        ({ newmod | curPage = Player }, Cmd.none)
                L.Close ->
                    ({ model | curPage = Player }, Cmd.none)

                L.NewSong ->
                    ({ model | editorModel = E.init, curPage = Editor }, Cmd.none)
                --_ ->
                --    let 
                --        (newmod, newcmd) = L.update submsg model.libraryModel
                --    in
                --        ({ model | libraryModel = newmod }
                --        , Cmd.map (\sm -> LibEvent sm) newcmd )

        ChangePage newp -> ({ model | curPage = newp }, Cmd.none)

        ResetDialog -> (resetDialog model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Tune.cursorChanged SetCursor
        , Tune.sequenceFinished (always (PpEvent Pp.SeqFinished)) ]

view : Model -> Html Msg
view model =
    div [] <|
        (
            case model.dialogBox of
                Nothing -> []
                Just db -> [ displayDialog db]
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

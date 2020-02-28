module Editor exposing (..)
import Generator as G
import PlayerPage as Pp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)

type alias Grid = Array { len : Float, chord : Maybe G.Chord }
type alias SubModel =
    { grid : Grid
    , tool : Tool
    , curChord : G.Chord
    , undoList : ModelList
    , redoList : ModelList
    , title : String
    , composer : String
    , defTempo : Float }

type ModelList = ModelList (List SubModel)
unwrap : ModelList -> List SubModel
unwrap ml = (\(ModelList l) -> l)(ml)

type Tool = SplitCase 
            | MergeWithBefore
            | SetChord
            | RemoveBar
            | SetToNothing
            | InsertBar


type SubMsg =
    CaseClicked Int
    | AppendBar
    | Quit
    | ConfirmQuit
    | SaveAndQuit
    | SetTool Tool
    | SetBase G.NoteName
    | SetAlteration G.Alteration
    | SetChordType G.ChordType
    | Undo
    | Redo
    | TitleChanged String
    | ComposerChanged String
    | TempoChanged Float

init = { grid = Array.fromList [ { len=4, chord=Nothing }
                               , { len=4, chord=Nothing }
                               , { len=4, chord=Nothing }
                               , { len=4, chord=Nothing }]
       , tool = SetChord
       , curChord = G.chord G.C G.Natural G.Dom7
       , undoList = ModelList []
       , redoList = ModelList []
       , title = "New song"
       , composer = "Unknown"
       , defTempo = 160 }
pushUndo : SubModel -> SubModel -> SubModel
pushUndo oldmod newmod =
    { newmod | undoList=ModelList (oldmod::unwrap(newmod.undoList)) }
pushRedo : SubModel -> SubModel -> SubModel
pushRedo oldmod newmod =
    { newmod | redoList=ModelList (oldmod::unwrap(newmod.redoList)) }
popUndo : SubModel -> SubModel
popUndo newmod =
    case unwrap(newmod.undoList) of
        [] -> newmod
        h::t -> h
popRedo : SubModel -> SubModel
popRedo newmod =
    case unwrap(newmod.redoList) of
        [] -> newmod
        h::t -> h

update : SubMsg -> SubModel -> (SubModel, Cmd SubMsg)
update msg model = 
    case msg of
        CaseClicked i -> 
            let 
                newmod = 
                    (case model.tool of
                        SetChord ->
                            { model | grid=setChord model.grid i (Just model.curChord) }
                        SplitCase ->
                            { model | grid=splitCell i model.grid }
                        MergeWithBefore ->
                            { model | grid=mergeCell i model.grid }
                        RemoveBar ->
                            { model | grid=remove i model.grid }
                        SetToNothing ->
                            { model | grid=setChord model.grid i Nothing }
                        InsertBar ->
                            { model | grid=insert (i-1) { len=4, chord=Nothing } model.grid}
                    )
            in 
                (pushUndo model newmod, Cmd.none)
        AppendBar -> 
            (let 
                newmod = { model | grid=Array.push { len=4, chord=Nothing } model.grid} 
            in 
                pushUndo model newmod
            , Cmd.none)
        SetTool t -> ({ model | tool=t }, Cmd.none)
        SetBase n ->
            let
                prevchord = model.curChord
                prevnote = prevchord.note
            in
                ({ model | curChord={prevchord | note={prevnote | name=n}}}, Cmd.none)
        SetAlteration n ->
            let
                prevchord = model.curChord
                prevnote = prevchord.note
            in
                ({ model | curChord={prevchord | note={prevnote | alt=n}}}, Cmd.none)
        SetChordType t ->
            let
                prevchord = model.curChord
            in
                ({ model | curChord={prevchord | type_=t}}, Cmd.none)
        Undo ->
            (pushRedo model (popUndo model), Cmd.none)
            --(popUndo model, Cmd.none)
        Redo ->
            (pushUndo model (popRedo model), Cmd.none)

        TitleChanged t ->
            ({ model | title = t}, Cmd.none)
        ComposerChanged c ->
            ({ model | composer = c}, Cmd.none)
        TempoChanged f ->
            ({ model | defTempo = f}, Cmd.none)


        _ -> (model, Cmd.none)

view : SubModel -> Html SubMsg
view model = div [] [ button [ onClick Quit ] [ text "quit editor" ]
                    , button [ onClick SaveAndQuit ] [ text "save and quit" ]
                    , button [ onClick AppendBar ] [ text "+1 bar" ]
                    , chordSelectBar (model.curChord)
                    , toolSelectBar (model.tool)
                    , button [ onClick AppendBar ] [ text "+1 bar" ]
                    , button [ onClick Undo ] [ text "Undo" ]
                    , button [ onClick Redo ] [ text "Redo" ]
                    , div [] [ text "Title: ", input [ type_ "text", value model.title, onInput TitleChanged ] [] ]
                    , div [] [ text "Composer: ", input [ type_ "text", value model.composer, onInput ComposerChanged ] [] ]
                    , div []
                        [ text "Default tempo: "
                        , input [ type_ "range"
                                , onInput (\s -> (case String.toFloat s of
                                                    Just f -> TempoChanged f
                                                    _ -> TempoChanged model.defTempo))
                                , Html.Attributes.min "30"
                                , Html.Attributes.max "300"
                                , value (String.fromFloat model.defTempo) 
                                , step "1" ] []
                        , text <| (String.fromFloat model.defTempo) ++ " BPM"
                        ]
                    , displayGrid model.grid ]

toolSelectBar : Tool -> Html SubMsg
toolSelectBar tool =
    let
        buttonAttr t = [ style "background-color" (if tool/=t then "#e7e7e7" else "#f44336")
                       , onClick (SetTool t) ]
    in
    div [ style "color" "white" ]
        [ button (buttonAttr SetChord) [ text "pen" ]
        , button (buttonAttr SplitCase) [ text "split" ]
        , button (buttonAttr MergeWithBefore) [ text "merge" ]
        , button (buttonAttr InsertBar) [ text "insert bar" ]
        , button (buttonAttr RemoveBar) [ text "remove bar" ]
        , button (buttonAttr SetToNothing) [ text "remove chord" ]
        ]

chordSelectBar : G.Chord -> Html SubMsg
chordSelectBar c =
    let 
        buttonAttr var t cmd =
            [ style "background-color" (if var/=t then "#e7e7e7" else "#f44336")
            , onClick (cmd t) ]
    in
    div [ style "color" "white" ]
        [ span []
               [ button (buttonAttr c.note.name G.C SetBase) [ text "C" ]
               , button (buttonAttr c.note.name G.D SetBase) [ text "D" ]
               , button (buttonAttr c.note.name G.E SetBase) [ text "E" ]
               , button (buttonAttr c.note.name G.F SetBase) [ text "F" ]
               , button (buttonAttr c.note.name G.G SetBase) [ text "G" ]
               , button (buttonAttr c.note.name G.A SetBase) [ text "A" ]
               , button (buttonAttr c.note.name G.B SetBase) [ text "B" ] ]
        , span [ style "padding" "20px" ]
               [ button (buttonAttr c.note.alt G.Natural SetAlteration) [ text "natural" ]
               , button (buttonAttr c.note.alt G.Sharp SetAlteration) [ text "#" ]
               , button (buttonAttr c.note.alt G.Flat SetAlteration) [ text "b" ] ]
        , span []
               [ button (buttonAttr c.type_ G.Dom7 SetChordType) [ text "7" ]
               , button (buttonAttr c.type_ G.Min7 SetChordType) [ text "-7" ]
               , button (buttonAttr c.type_ G.Maj7 SetChordType) [ text "∆" ]
               , button (buttonAttr c.type_ G.Alt7 SetChordType) [ text "7alt" ]
               , button (buttonAttr c.type_ G.Dom7b9 SetChordType) [ text "7b9" ]
               , button (buttonAttr c.type_ G.Dom7s5 SetChordType) [ text "7#5" ]
               , button (buttonAttr c.type_ G.Min7b5 SetChordType) [ text "-7b5" ]
               , button (buttonAttr c.type_ G.Dim SetChordType) [ text "o" ]
               , button (buttonAttr c.type_ G.MinMaj SetChordType) [ text "-∆" ]
               , button (buttonAttr c.type_ G.Maj7s5 SetChordType) [ text "∆#5" ]
               , button (buttonAttr c.type_ G.NA SetChordType) [ text "NA" ]]]

               


chordprog2grid : Pp.SubModel -> Grid
chordprog2grid model = Array.fromList 
                        (List.map 
                            (\x -> { len = x.len, chord = (Tuple.first x.chord) })
                            (Pp.chordprog2grid model))

displayGrid : Grid -> Html SubMsg
displayGrid g =
    div [ class "realbook" ]
        (List.map
            (\(i, c) -> div
                [ style "width" <| (String.fromInt (floor (c.len*50)))++"px"
                , style "height" "100px"
                , style "display" "inline-block"
                , onClick (CaseClicked i) ]
                [ text  (Pp.chord2text c.chord) ])
            (Array.toIndexedList g)
        )

insert : Int -> a -> Array a -> Array a
insert i v a =
    let
        before = Array.slice 0 (i+1) a
        after = Array.slice (i+1) (Array.length a) a
    in
        Array.append (Array.push v before) after

remove : Int -> Array a -> Array a
remove i a =
    let
        before = Array.slice 0 i a
        after = Array.slice (i+1) (Array.length a) a
    in
        Array.append before after

splitCell : Int -> Grid -> Grid
splitCell i g =
    let
        mprevcell = (Array.get i g)
    in
        case mprevcell of
            Nothing -> g
            Just prevcell ->
                let
                    prevlen = prevcell.len
                    half = prevlen/2
                    newg = Array.set i {prevcell | len = toFloat <| ceiling half} g
                in
                    insert i { len = toFloat <| floor half, chord = Nothing } newg

mergeCell : Int -> Grid -> Grid
mergeCell i g =
    case Array.get i g of
        Nothing -> g
        Just cellI -> case Array.get (i-1) g of
            Nothing -> g --remove i g
            Just cellIm1 ->
                Array.set (i-1) { cellIm1 | len=cellIm1.len + cellI.len } <|
                    remove i g

setChord : Grid -> Int -> Maybe G.Chord -> Grid
setChord g i c =
    case Array.get i g of
        Nothing -> g
        Just cell -> --let prevc = cell.chord in
            Array.set i { cell | chord=c } g


grid2chordprog : Grid -> G.ChordProg
grid2chordprog g =
    let
        foldrec l time =
            case l of
                [] -> []
                h::t -> 
                    let
                        --newtime = time+h.len
                        following = foldrec t (time+h.len)
                    in
                        case h.chord of
                            Nothing -> following
                            Just c -> { time=time, chord=c }::following
        glist = Array.toList g
        chords = foldrec glist 0
        end = List.foldl (\c f -> f+c.len) 0 glist
    in
        { chords=chords, end=end }
                    

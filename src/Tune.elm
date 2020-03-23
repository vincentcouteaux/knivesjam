port module Tune exposing ( play
                          , pause
                          , cursorChanged
                          , setCursor
                          , setBpm
                          , sequenceFinished
                          , setSequence
                          , setInstVolume
                          , auclairdelalune
                          , Sequence
                          , Event)

port play : () -> Cmd msg

port pause : () -> Cmd msg

port setCursor : Float -> Cmd msg

port setBpm : Float -> Cmd msg

port setSequence : Sequence -> Cmd msg

port setInstVolume : (String, Float) -> Cmd msg

port cursorChanged : (Float -> msg) -> Sub msg

port sequenceFinished : (() -> msg) -> Sub msg

type alias Event =
    { time : Float
    , onset : Bool
    , pitch : Int
    , instrument : String
    , gain : Float }

type alias Sequence = List Event

auclairdelalune =
    [ Event 0 True 55 "sineBuf" 1
    , Event 0.5 False 55 "sineBuf" 1
    , Event 1 True 55 "sineBuf" 1
    , Event 1.5 False 55 "sineBuf" 1
    , Event 2 True 55 "sineBuf" 1
    , Event 2.5 False 55 "sineBuf" 1
    , Event 3 True 57 "sineBuf" 1
    , Event 3.5 False 57 "sineBuf" 1
    , Event 8 True 55 "sineBuf" 1
    , Event 8.5 False 55 "sineBuf" 1
    , Event 9 True 59 "sineBuf" 1
    , Event 9.5 False 59 "sineBuf" 1
    , Event 10 True 57 "sineBuf" 1
    , Event 10.5 False 57 "sineBuf" 1
    , Event 4 True 59 "sineBuf" 1
    , Event 5.5 False 59 "sineBuf" 1
    , Event 6 True 57 "sineBuf" 1
    , Event 7.5 False 57 "sineBuf" 1
    , Event 11 True 57 "sineBuf" 1
    , Event 11.5 False 57 "sineBuf" 1
    , Event 12 True 55 "sineBuf" 1
    , Event 14.5 False 55 "sineBuf" 1 ]

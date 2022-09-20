module Main exposing (..)

-- MIT License
-- Copyright (c) 2022 Gavin Gray

import Array exposing (Array)
import Browser
import Color exposing (Color)
import File exposing (File)
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Svg
import Svg.Attributes as SvgAttr
import Task


type alias PersonalFile =
    { buf : File, content : String }


type alias Workspace =
    { files : List PersonalFile }


type alias Color =
    { r : Int, g : Int, b : Int, a : Int }


type alias Style =
    { foreground : Color }


type alias SourceOutput =
    { filename : String
    , styles : Array Style
    , enriched_toks : List (List (String, Int)) }


type alias SourceRequest =
    { filename : String }


type alias SourceResponse =
    Result String SourceOutput



-- JSON ENCODERS


sourceEncoder : SourceRequest -> JE.Value
sourceEncoder req =
    JE.object
        [ ("filename", JE.string req.filename)
        ]


-- JSON DECODERS


colorDecoder : JD.Decoder Color
colorDecoder =
    JD.map4 Color
        (JD.field "r" JD.int)
        (JD.field "g" JD.int)
        (JD.field "b" JD.int)
        (JD.field "a" JD.int)


styleDecoder : JD.Decoder Style
styleDecoder =
    JD.map Style
        (JD.field "foreground" colorDecoder)


tupleStringIntDecoder : JD.Decoder (String, Int)
tupleStringIntDecoder =
    JD.map2 Tuple.pair
        (JD.index 0 JD.string)
        (JD.index 1 JD.int)


sourceOutputDecoder : JD.Decoder SourceOutput
sourceOutputDecoder =
    JD.map3 SourceOutput
        (JD.field "filename" JD.string)
        (JD.field "styles" (JD.array styleDecoder))
        (JD.field "enriched_toks" (JD.list (JD.list tupleStringIntDecoder)))


sourceResponseDecoder : JD.Decoder SourceResponse
sourceResponseDecoder = resultDecoder JD.string sourceOutputDecoder


resultDecoder : (JD.Decoder err) -> (JD.Decoder ok) -> JD.Decoder (Result err ok)
resultDecoder f g =
    JD.andThen
        (\mybOk ->
             case mybOk of
                 Nothing -> JD.map Err (JD.field "Err" f)
                 Just v -> JD.succeed (Ok v))
       (JD.maybe (JD.field "Ok" g))


-- MODEL


type Model
    = NoWorkspace
    | Loading String
    | SimpleMessage String
    | OpenFile SourceOutput


init : () -> (Model, Cmd Msg)
init _ = (NoWorkspace, Cmd.none)


-- UPDATE


type Msg
  = GetSource String
  | ReceivedSource (Result Http.Error SourceResponse)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetSource fn ->
            (Loading fn
            , Http.post
                 { url = "http://127.0.0.1:8008/example-source"
                 , body = Http.jsonBody (sourceEncoder { filename = fn })
                 , expect = Http.expectJson ReceivedSource sourceResponseDecoder })
        ReceivedSource res ->
            case res of
                Ok (Ok src) -> (OpenFile src, Cmd.none)
                Ok (Err m) -> (SimpleMessage m, Cmd.none)
                Err (Http.BadUrl s) ->
                    (SimpleMessage ("[ERROR] bad URL " ++ s), Cmd.none)
                Err Http.Timeout ->
                    (SimpleMessage "[ERROR] request timed out", Cmd.none)
                Err Http.NetworkError ->
                    (SimpleMessage "[ERROR] network error", Cmd.none)
                Err (Http.BadStatus e) ->
                    (SimpleMessage ("[ERROR] post exited with " ++ (String.fromInt e)), Cmd.none)
                Err (Http.BadBody s) ->
                    (SimpleMessage ("[ERROR] bad body " ++ s), Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    case model of
      NoWorkspace -> div [] [ button [ onClick (GetSource "/Users/gavinleroy/dev/prj/aquascope/files/hello/src/main.rs") ]
                                  [ text "Send Post" ] ]
      Loading fn -> div [] [ text ("Loading file ... " ++ fn) ]
      SimpleMessage msg -> div [] [ text msg ]
      OpenFile src -> viewOpenFile src


-- UTILITIES


viewOpenFile : SourceOutput -> Html Msg
viewOpenFile { filename , styles , enriched_toks } =
    let colorAttributes = Array.map
                          (\{ foreground } ->
                               Color.rgb255 foreground.r foreground.g foreground.b
                          |> Color.toCssString
                          |> SvgAttr.fill) styles
    in
    let defaultColor = SvgAttr.fill "black" in
    let getAttr i = Maybe.withDefault defaultColor (Array.get i colorAttributes) in
    let noBreakSpace = '\u{00A0}' in
    let preserveWhitespace = String.map (\c -> if c == ' ' then noBreakSpace else c) in
    Svg.svg [ SvgAttr.viewBox "-100 -100 1024 1024" ]
        (List.indexedMap
             (\i line ->
                  Svg.text_ [ String.fromInt ((i + 1) * 20) |> SvgAttr.dy
                            , SvgAttr.fontFamily "Fantasque Sans Mono"]
                  (List.map
                       (\tok ->
                            Svg.tspan
                            [ getAttr <| Tuple.second tok ]
                            [ Svg.text <| preserveWhitespace <| Tuple.first tok ]
                       ) line)) enriched_toks)


-- MAIN


main =
    Browser.element
      { init = init
      , update = update
      , view = view
      , subscriptions = subscriptions
      }

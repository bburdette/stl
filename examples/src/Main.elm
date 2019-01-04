module Main exposing (Flags, Model, Msg(..), getstl, init, main, update, view)

import Browser
import Html exposing (Html)
import Http
import Math.Vector3 as Vec3 exposing (Vec3)
import Stl


type alias Flags =
    { location : String
    , width : Int
    , height : Int
    }


type alias Model =
    { mbt : Maybe Stl.Triangles }


type Msg
    = BinaryResponse (Result Http.Error Stl.Triangles)


getstl : String -> String -> Cmd Msg
getstl location stlfile =
    Http.get
        { url = Debug.log "url: " <| location ++ "/stl/" ++ stlfile
        , expect = Http.expectBytes BinaryResponse Stl.binaryStl
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        _ =
            Debug.log "flags" flags

        location =
            String.replace "main.html" "" flags.location
    in
    ( { mbt = Nothing }, getstl location "Test_(aka).stl" )


vec3String : Vec3 -> String
vec3String v3 =
    let
        r =
            Vec3.toRecord v3
    in
    "x: "
        ++ String.fromFloat r.x
        ++ "y: "
        ++ String.fromFloat r.y
        ++ "z: "
        ++ String.fromFloat r.z


triangleString : Stl.Triangle -> String
triangleString t =
    "normal: "
        ++ vec3String t.normal
        ++ (case t.vertices of
                ( v1, v2, v3 ) ->
                    " v1: "
                        ++ vec3String v1
                        ++ " v2: "
                        ++ vec3String v2
                        ++ " v3: "
                        ++ vec3String v3
           )


view : Model -> Html Msg
view model =
    case model.mbt of
        Nothing ->
            Html.text "no model loaded!  try examples/build.sh and then open main.html"

        Just triangles ->
            Html.div [] <|
                List.map
                    (\t ->
                        Html.p [] [ Html.text (triangleString t) ]
                    )
                    triangles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BinaryResponse triangles ->
            let
                _ =
                    Debug.log "triangles: " triangles
            in
            case triangles of
                Ok t ->
                    ( { model | mbt = Just t }, Cmd.none )

                Err e ->
                    ( { model | mbt = Nothing }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

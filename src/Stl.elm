module Stl exposing (Triangles, binaryStl, loopNumbers, makeThing1, makeThing2, makeThing3, numsToTriangle, numsToVec3)

--import Bytes.Encode as BE exposing (Encoder)

import Bytes exposing (Endianness(..))
import Bytes.Decode as BD exposing (Decoder, Step)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type alias Triangle =
    { normal : Vec3
    , vertices : ( Vec3, Vec3, Vec3 )
    }


type alias Triangles =
    List Triangle


rest : List a -> List a
rest list =
    case List.tail list of
        Nothing ->
            []

        Just elts ->
            elts


makeThing3 : (a -> a -> a -> b) -> List a -> Maybe ( b, List a )
makeThing3 f lst =
    Maybe.map f (List.head lst)
        |> Maybe.andThen (\g -> makeThing2 g (rest lst))


makeThing2 : (a -> a -> b) -> List a -> Maybe ( b, List a )
makeThing2 f lst =
    Maybe.map f (List.head lst)
        |> Maybe.andThen (\g -> makeThing1 g (rest lst))


makeThing1 : (a -> b) -> List a -> Maybe ( b, List a )
makeThing1 f lst =
    Maybe.map f (List.head lst)
        |> Maybe.andThen (\fb -> Just ( fb, rest lst ))


numsToVec3 : List Float -> Maybe ( Vec3, List Float )
numsToVec3 nums =
    makeThing3 vec3 nums


numsToTriangle : List Float -> Maybe Triangle
numsToTriangle floats =
    numsToVec3 floats
        |> Maybe.andThen
            (\( v1, flts ) ->
                numsToVec3 flts
                    |> Maybe.andThen
                        (\( v2, flts2 ) ->
                            numsToVec3 flts2
                                |> Maybe.andThen
                                    (\( v3, flts3 ) ->
                                        numsToVec3 flts3
                                            |> Maybe.andThen
                                                (\( v4, flts4 ) ->
                                                    Just { normal = v1, vertices = ( v2, v3, v4 ) }
                                                )
                                    )
                        )
            )


{-| Binary decoder for Stl files!
-}
binaryStl : Decoder Triangles
binaryStl =
    BD.bytes 80
        -- 80 bytes of we-don't-care
        |> BD.andThen
            (\_ ->
                BD.unsignedInt32 LE
                    -- followed by number of triangles
                    |> BD.andThen
                        (\count ->
                            BD.loop
                                ( count, [] )
                                loopTriangle
                        )
            )


loopTriangle : ( Int, Triangles ) -> Decoder (Step ( Int, Triangles ) Triangles)
loopTriangle ( count, triangles ) =
    if count == 0 then
        -- reverse the reversed list at the end.
        BD.succeed (BD.Done <| List.reverse triangles)

    else
        -- first get 12 float32s
        BD.loop ( 12, [] )
            loopNumbers
            |> BD.andThen
                (\t ->
                    -- then ignore 2 bytes
                    BD.bytes 2
                        |> BD.andThen
                            (\_ ->
                                -- build the list in reverse order because its faster.
                                case numsToTriangle t of
                                    Just tri ->
                                        BD.succeed (BD.Loop ( count - 1, tri :: triangles ))

                                    Nothing ->
                                        BD.fail
                            )
                )


loopNumbers : ( Int, List Float ) -> Decoder (Step ( Int, List Float ) (List Float))
loopNumbers ( count, nums ) =
    if count == 0 then
        BD.succeed (BD.Done (List.reverse nums))

    else
        BD.float32 LE
            |> BD.andThen
                (\num ->
                    BD.succeed (BD.Loop ( count - 1, num :: nums ))
                )

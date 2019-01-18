module Stl exposing
    ( Triangle
    , Triangles
    , binaryStl
    )

{-| A parser for binary STL files - the 3d models that are just a list of triangles.
Pretty much went by wikipedia on the STL format and tested on a few models.
No special STL features supported, just the basic format which is:

  - 80 bytes of stuff-we-ignore
  - int32 triangle count.
  - float32s - 12 for each triangle - normal, vertex 1, vertex 2, vertex 3
  - after each triangle, 2 bytes are ignored.


# Types

@docs Triangle
@docs Triangles


# Functions

@docs binaryStl

-}

import Bytes exposing (Endianness(..))
import Bytes.Decode as BD exposing (Decoder, Step)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| A Triangle is a normal Vec3 and a triple of Vec3 for the vertices
-}
type alias Triangle =
    { normal : Vec3
    , vertices : ( Vec3, Vec3, Vec3 )
    }


{-| Triangles is a list of Triangle
-}
type alias Triangles =
    List Triangle


rest : List a -> List a
rest list =
    case List.tail list of
        Nothing ->
            []

        Just elts ->
            elts


andMap : BD.Decoder a -> BD.Decoder (a -> b) -> BD.Decoder b
andMap next current =
    BD.map2 (<|) current next


decodeVec3 : Decoder Vec3
decodeVec3 =
    BD.succeed vec3
        |> andMap (BD.float32 LE)
        |> andMap (BD.float32 LE)
        |> andMap (BD.float32 LE)


decodeTriangle : Decoder Triangle
decodeTriangle =
    BD.succeed (\normal v1 v2 v3 -> { normal = normal, vertices = ( v1, v2, v3 ) })
        |> andMap decodeVec3
        |> andMap decodeVec3
        |> andMap decodeVec3
        |> andMap decodeVec3


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


makeThing1x : (a -> b) -> List a -> Maybe ( b, List a )
makeThing1x f lst =
    Maybe.map f (List.head lst)
        |> Maybe.andThen (\fb -> Just ( fb, rest lst ))


numsToVec3 : List Float -> Maybe ( Vec3, List Float )
numsToVec3 nums =
    makeThing1 vec3 nums
        |> Maybe.andThen
            (\( f, l ) ->
                makeThing1 f l
                    |> Maybe.andThen
                        (\( g, m ) ->
                            makeThing1 g m
                        )
            )


{-| Binary decoder for Stl files.
-}
binaryStl : Decoder Triangles
binaryStl =
    -- 80 bytes of we-don't-care
    BD.bytes 80
        |> BD.andThen
            (\_ ->
                -- followed by the number of triangles
                BD.unsignedInt32 LE
                    |> BD.andThen
                        (\count ->
                            -- and finally the triangles themselves
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
        -- first get 12 float32s (to make 1 triangle)
        decodeTriangle
            |> BD.andThen
                (\t ->
                    -- then ignore 2 bytes
                    BD.bytes 2
                        |> BD.andThen
                            (\_ ->
                                BD.succeed (BD.Loop ( count - 1, t :: triangles ))
                            )
                )

module Main exposing (main)

import Array
import Browser
import Element as El
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Matrix as Mt
import Round


type alias Dimensions =
    { tileSize : Int
    , xSizeTiles : Int
    , ySizeTiles : Int
    , zSizeTiles : Int
    }


type alias BlenderSettings =
    { width : Int
    , height : Int
    , scale : Float
    }


type alias Volume =
    List Mt.Matrix


rotateZ : Float -> Mt.Matrix
rotateZ amount =
    let
        radians =
            degrees amount
    in
    Mt.from2DList
        [ [ cos radians, -(sin radians), 0 ]
        , [ sin radians, cos radians, 0 ]
        , [ 0, 0, 1 ]
        ]


rotateX : Float -> Mt.Matrix
rotateX amount =
    let
        radians =
            degrees amount
    in
    Mt.from2DList
        [ [ 1, 0, 0 ]
        , [ 0, cos radians, -(sin radians) ]
        , [ 0, sin radians, cos radians ]
        ]


project : Mt.Matrix
project =
    Mt.mul (rotateX 60) (rotateZ 45)


createVolume : Float -> Float -> Float -> Volume
createVolume x y z =
    let
        xs =
            x / 2

        ys =
            y / 2

        zs =
            z / 2
    in
    [ Mt.from2DList [ [ -xs ], [ -ys ], [ -zs ] ]
    , Mt.from2DList [ [ xs ], [ -ys ], [ -zs ] ]
    , Mt.from2DList [ [ -xs ], [ ys ], [ -zs ] ]
    , Mt.from2DList [ [ xs ], [ ys ], [ -zs ] ]
    , Mt.from2DList [ [ -xs ], [ -ys ], [ zs ] ]
    , Mt.from2DList [ [ xs ], [ -ys ], [ zs ] ]
    , Mt.from2DList [ [ -xs ], [ ys ], [ zs ] ]
    , Mt.from2DList [ [ xs ], [ ys ], [ zs ] ]
    ]


getY : Mt.Matrix -> Maybe Float
getY mat =
    Mt.get ( 2, 1 ) mat


getX : Mt.Matrix -> Maybe Float
getX mat =
    Mt.get ( 1, 1 ) mat


toList : Maybe a -> List a
toList a =
    Maybe.map (\e -> [ e ]) a |> Maybe.withDefault []


size : (Mt.Matrix -> Maybe Float) -> Volume -> Maybe Float
size dimension volume =
    Maybe.map2 (\a -> \b -> a - b)
        (volume |> List.map dimension |> List.concatMap toList |> List.maximum)
        (volume |> List.map dimension |> List.concatMap toList |> List.minimum)



-- Calculate the orthographic scale for an image that is taller than it is wide
-- for this to be the case the Z size of the image is what we need to use
-- So we find out what proportion of the height is given over to the sides of the volume
-- then create a scale that would make the sides take up the whole image before reducing them
-- by the proportion we actually want the sides to take up


calculateScaleFromZ : Volume -> Dimensions -> Maybe Float
calculateScaleFromZ volume dimensions =
    let
        volArray =
            Array.fromList volume

        projectedCube =
            createVolume 1 1 1
                |> List.map (Mt.mul project)
                |> Array.fromList

        heightFactor =
            Maybe.map2 (\a -> \b -> a - b)
                (projectedCube |> Array.get 0 |> Maybe.andThen getY)
                (projectedCube |> Array.get 4 |> Maybe.andThen getY)

        proportionOfZ =
            Maybe.map3 (\a -> \b -> \height -> (a - b) / height)
                (volArray |> Array.get 0 |> Maybe.andThen getY)
                (volArray |> Array.get 4 |> Maybe.andThen getY)
                (size getY volume)
    in
    Maybe.map2
        (\heightFact -> \propZ -> (toFloat dimensions.zSizeTiles * heightFact) / propZ)
        heightFactor
        proportionOfZ



-- Calculate the orthographic scale for an image that is wider than it is tall
-- We take the longest side of the image and because the image is rotated we need to scale down
-- each side so the hypotenuse across the top of the volume is equal to the image width


calculateScaleFromX : Dimensions -> Float
calculateScaleFromX dimensions =
    let
        xSize =
            toFloat dimensions.xSizeTiles

        ySize =
            toFloat dimensions.ySizeTiles

        maxDimension =
            max xSize ySize

        hypotenuseRatio = 
            sqrt 2

    in
    hypotenuseRatio + ((hypotenuseRatio / 2) * (maxDimension - 1))



calculateSettings : Dimensions -> BlenderSettings
calculateSettings dimensions =
    let
        sideLength =
            toFloat dimensions.tileSize / sqrt 2

        volume =
            createVolume
                (toFloat dimensions.xSizeTiles * sideLength)
                (toFloat dimensions.ySizeTiles * sideLength)
                (toFloat dimensions.zSizeTiles * sideLength)
                |> List.map (Mt.mul project)

        width =
            size getX volume |> Maybe.withDefault 0

        height =
            size getY volume |> Maybe.withDefault 0

        scale =
            if width > height then
                calculateScaleFromX dimensions

            else
                calculateScaleFromZ volume dimensions
                    |> Maybe.withDefault 0
    in
    { scale = scale
    , width = round width
    , height = round height
    }


type SetDimension
    = SetTileSize Int
    | SetXSizeTiles Int
    | SetYSizeTiles Int
    | SetZSizeTiles Int


initialDimensions : Dimensions
initialDimensions =
    { tileSize = 32
    , xSizeTiles = 1
    , ySizeTiles = 1
    , zSizeTiles = 1
    }


updateDimensions : SetDimension -> Dimensions -> ( Dimensions, Cmd msg )
updateDimensions setter dim =
    case setter of
        SetTileSize what ->
            ( { dim | tileSize = what }, Cmd.none )

        SetXSizeTiles what ->
            ( { dim | xSizeTiles = what }, Cmd.none )

        SetYSizeTiles what ->
            ( { dim | ySizeTiles = what }, Cmd.none )

        SetZSizeTiles what ->
            ( { dim | zSizeTiles = what }, Cmd.none )


field : String -> (Int -> SetDimension) -> Int -> El.Element SetDimension
field label fn value =
    El.el
        [ El.paddingXY 0 5
        ]
        (Input.text
            [ Font.size 20
            , El.paddingXY 5 10
            ]
            { label = Input.labelAbove [ El.centerY, El.width El.fill ] (El.text label)
            , onChange = \e -> String.toInt e |> Maybe.map fn |> Maybe.withDefault (fn 0)
            , text = String.fromInt value
            , placeholder = Nothing
            }
        )


heading : String -> El.Element msg
heading what =
    El.el
        [ Font.size 28
        , Font.bold
        ]
        (El.text what)


resultRow : String -> String -> El.Element msg
resultRow title contents =
    El.row
        [ Border.dotted
        , Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
        , El.width El.fill
        , Border.color (El.rgb255 96 96 96)
        , El.paddingXY 0 5
        ]
        [ El.el [] (El.text title)
        , El.el [ El.alignRight, Font.bold ] (El.text contents)
        ]


result : Dimensions -> El.Element msg
result dims =
    let
        settings =
            calculateSettings dims
    in
    El.column [ El.width El.fill, El.paddingXY 0 40, El.spacingXY 0 10 ]
        [ heading "Blender Settings"
        , resultRow "Image Width" (String.fromInt settings.width)
        , resultRow "Image Height" (String.fromInt settings.height)
        , resultRow "Ortho Scale" (settings.scale |> Round.round 5)
        ]


view : Dimensions -> Html SetDimension
view dimensions =
    El.layout []
        (El.column
            [ El.centerX
            , El.centerY
            , El.spacingXY 0 10
            ]
            [ heading "Inputs"
            , field "Tile Size (px)" SetTileSize dimensions.tileSize
            , field "X Size (tiles)" SetXSizeTiles dimensions.xSizeTiles
            , field "Y Size (tiles)" SetYSizeTiles dimensions.ySizeTiles
            , field "Z Size (tiles)" SetZSizeTiles dimensions.zSizeTiles
            , result dimensions
            ]
        )


main : Program () Dimensions SetDimension
main =
    Browser.element
        { init = \_ -> ( initialDimensions, Cmd.none )
        , update = updateDimensions
        , subscriptions = \_ -> Sub.none
        , view = view
        }

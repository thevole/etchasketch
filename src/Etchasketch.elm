-- module Etchasketch exposing (..)


module Main exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Keyboard.Extra
import Time exposing (Time, second)
import AnimationFrame
import Animation exposing (..)


type alias Model =
    { points : List Point
    , x : Int
    , y : Int
    , keyboardModel : Keyboard.Extra.Model
    , clock : Time
    , animation : Animation
    , animations : List (Time -> Animation)
    , color : Color
    }


type alias Point =
    ( Int, Int )


shakeAnimation : Time -> Animation
shakeAnimation t =
    animation t
        |> from 0
        |> to 40
        |> duration (500 * Time.millisecond)


shakeAnimation' : Time -> Animation
shakeAnimation' t =
    animation t
        |> from 40
        |> to -20
        |> duration (500 * Time.millisecond)


shakeAnimation'' : Time -> Animation
shakeAnimation'' t =
    animation t
        |> from -20
        |> to 10
        |> duration (500 * Time.millisecond)


shakeAnimation''' : Time -> Animation
shakeAnimation''' t =
    animation t
        |> from 10
        |> to 0
        |> duration (500 * Time.millisecond)


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init
    in
        ( { points = [ ( 0, 0 ) ]
          , x = 0
          , y = 0
          , keyboardModel = keyboardModel
          , clock = 0
          , animation = static 0
          , animations = []
          , color = red
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )


animations : List (Time -> Animation)
animations =
    [ shakeAnimation
    , shakeAnimation'
    , shakeAnimation''
    , shakeAnimation'''
    ]


shakeButton : Html Msg
shakeButton =
    Html.button [ onClick Shake ] [ Html.text "Shake it good" ]


colorButton : Color -> String -> Html Msg
colorButton c txt =
    Html.button [ onClick (SetColor c) ] [ Html.text txt ]


view : Model -> Html Msg
view model =
    let
        angle =
            animate model.clock model.animation
    in
        div []
            [ collage 800
                600
                [ (rotate (degrees angle) (drawLine model.color model.points)) ]
                |> Element.toHtml
            , shakeButton
            , colorButton blue "Blue"
            , colorButton red "Red"
            , colorButton green "Green"
            , colorButton purple "Purple"
            , colorButton black "Black"
            ]


drawLine : Color -> List Point -> Form
drawLine color points =
    let
        intsToFloats : ( Int, Int ) -> ( Float, Float )
        intsToFloats ( x, y ) =
            ( toFloat x, toFloat y )

        shape =
            path (List.map intsToFloats points)
    in
        shape
            |> traced (solid color)


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , AnimationFrame.diffs Tick
        ]


type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time
    | Shake
    | SetColor Color


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetColor c ->
            { model | color = c } ! []

        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.Extra.update keyMsg model.keyboardModel
            in
                ( { model | keyboardModel = keyboardModel }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

        Tick dt ->
            let
                { x, y } =
                    Keyboard.Extra.arrows model.keyboardModel

                newX =
                    model.x + x

                newY =
                    model.y + y

                newClock =
                    model.clock + dt

                ( newPoints, newAnimation, newAnimations ) =
                    case (isDone model.clock model.animation) of
                        True ->
                            let
                                nextAnimation =
                                    case List.head model.animations of
                                        Just animation ->
                                            animation model.clock

                                        Nothing ->
                                            static 0

                                nextAnimations =
                                    (List.tail model.animations) |> Maybe.withDefault ([])

                                justFinished =
                                    nextAnimation
                                        `equals` (static 0)
                                        && not (model.animation `equals` (static 0))

                                nextPoints =
                                    case justFinished of
                                        True ->
                                            []

                                        False ->
                                            model.points
                            in
                                ( nextPoints, nextAnimation, nextAnimations )

                        False ->
                            ( model.points, model.animation, model.animations )

                newPoints' =
                    case ( x, y ) of
                        ( 0, 0 ) ->
                            newPoints

                        _ ->
                            ( newX, newY ) :: newPoints

                model' =
                    { model
                        | points = newPoints'
                        , clock = newClock
                        , animation = newAnimation
                        , animations = newAnimations
                    }
            in
                case ( x, y ) of
                    ( 0, 0 ) ->
                        model' ! []

                    _ ->
                        { model'
                            | x = newX
                            , y = newY
                        }
                            ! []

        Shake ->
            { model
                | animations = animations
            }
                ! []


keyUp : Keyboard.KeyCode -> Model -> Model
keyUp keyCode model =
    case keyCode of
        38 ->
            -- up
            { model | y = model.y + 1, points = ( model.x, model.y + 1 ) :: model.points }

        40 ->
            -- down
            { model | y = model.y - 1, points = ( model.x, model.y - 1 ) :: model.points }

        37 ->
            -- left
            { model | x = model.x - 1, points = ( model.x - 1, model.y ) :: model.points }

        39 ->
            -- right
            { model | x = model.x + 1, points = ( model.x + 1, model.y ) :: model.points }

        _ ->
            model

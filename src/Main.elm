module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import AnimationFrame
import Keyboard exposing (KeyCode)
import Time exposing (Time)
import String exposing (..)


---- MODEL ----


type alias Entity =
    { x : Float
    , y : Float
    , direction : Direction
    , inMotion : Bool
    , currentFrame : String
    }


type Direction
    = Left
    | Right


type alias Model =
    { charactersPath : String
    , elapsedTime : Float
    , mario : Entity
    , keyPressed : String
    }


type alias Flags =
    { charactersPath : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { charactersPath = flags.charactersPath
      , elapsedTime = 0
      , mario = { x = 0, y = 0, direction = Left, inMotion = False, currentFrame = "275 44 16 16" }
      , keyPressed = "Nothing pressed"
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | TimeUpdate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            let
                updatedModel =
                    { model | elapsedTime = model.elapsedTime + (dt / 1000) }
            in
                ( { updatedModel | mario = moveMario dt model.keyPressed model.mario }, Cmd.none )

        KeyDown keyCode ->
            ( { model | keyPressed = toString keyCode }, Cmd.none )

        KeyUp keyCode ->
            let
                oldMario =
                    model.mario

                newMario =
                    { oldMario | inMotion = False }
            in
                ( { model
                    | keyPressed = "Nothing pressed"
                    , mario = newMario
                  }
                , Cmd.none
                )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        ticker =
            (round model.elapsedTime) % 10
    in
        Html.div []
            [ text model.keyPressed
            , svg
                [ width "100%"
                , height "100%"
                , viewBox "0 0 640 400"
                ]
                [ drawMario model.mario model.charactersPath ]
            ]


moveMario : Time -> String -> Entity -> Entity
moveMario dt keyPressed mario =
    let
        leftArrow =
            "37"

        rightArrow =
            "39"

        marioRightSpriteFrameOne =
            "275 44 16 16"

        marioRightSpriteFrameTwo =
            "290 44 16 16"
    in
        if keyPressed == leftArrow then
            { mario
                | x = mario.x - dt / 10
                , inMotion = True
                , direction = Left
            }
        else if keyPressed == rightArrow && mario.currentFrame == marioRightSpriteFrameOne then
            { mario
                | x = mario.x + dt / 10
                , inMotion = True
                , direction = Right
                , currentFrame = marioRightSpriteFrameTwo
            }
        else if keyPressed == rightArrow && mario.currentFrame == marioRightSpriteFrameTwo then
            { mario
                | x = mario.x + dt / 10
                , inMotion = True
                , direction = Right
                , currentFrame = marioRightSpriteFrameOne
            }
        else
            mario


animationTicker : Float -> Int
animationTicker currentPosition =
    Debug.log "frame" ((round (currentPosition / 50)) % 3)


marioSpriteRight mario =
    let
        ticker =
            animationTicker mario.x

        marioRightSpriteFrameOne =
            "275 44 16 16"

        marioRightSpriteFrameTwo =
            "290 44 16 16"

        marioRightSpriteFrameThree =
            "305 44 16 16"
    in
        if ticker == 0 then
            { mario
                | currentFrame = marioRightSpriteFrameOne
            }
        else if ticker == 1 then
            { mario
                | currentFrame = marioRightSpriteFrameTwo
            }
        else if ticker == 2 then
            { mario
                | currentFrame = marioRightSpriteFrameThree
            }
        else
            mario


marioSpriteLeft : Entity -> Entity
marioSpriteLeft mario =
    let
        ticker =
            animationTicker mario.x

        marioRightSpriteFrameOne =
            "275 44 16 16"

        marioRightSpriteFrameTwo =
            "290 44 16 16"

        marioRightSpriteFrameThree =
            "305 44 16 16"
    in
        if ticker == 1 then
            { mario
                | currentFrame = marioRightSpriteFrameOne
            }
        else if ticker == 2 then
            { mario
                | currentFrame = marioRightSpriteFrameTwo
            }
        else
            { mario
                | currentFrame = marioRightSpriteFrameThree
            }


toPx : Int -> String
toPx number =
    (toString number) ++ "px"


spriteWindow spriteXcoord spriteYcoord spriteWidth spriteHeight =
    let
        viewBoxParams =
            spriteXcoord :: spriteYcoord :: spriteWidth :: spriteHeight :: []
    in
        String.join " " viewBoxParams


drawSprite entity spritesPath =
    let
        spriteHeight =
            16

        spriteWidth =
            16
    in
        svg
            [ x (toString entity.x)
            , y (toString entity.y)
            , width (toPx spriteWidth)
            , height (toPx spriteHeight)
            , viewBox "321 44 16 16"
            , version "1.1"
            ]
            [ image
                [ x "0px"
                , y "0px"
                , width "513px"
                , height "401px"
                , xlinkHref spritesPath
                ]
                []
            ]


drawMario : Entity -> String -> Svg Msg
drawMario mario spritesPath =
    let
        spriteWidth =
            16

        spriteHeight =
            16

        marioLeftSprite =
            "222 44 16 16"

        spriteDirection =
            case mario.direction of
                Left ->
                    marioSpriteLeft mario

                Right ->
                    marioSpriteRight mario
    in
        svg
            [ x (toString mario.x)
            , y (toString mario.y)
            , width (toPx spriteWidth)
            , height (toPx spriteHeight)
            , viewBox mario.currentFrame
            , version "1.1"
            ]
            [ image
                [ imageRendering "pixelated"
                , x "0px"
                , y "0px"
                , width "513px"
                , height "401px"
                , xlinkHref spritesPath
                ]
                []
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

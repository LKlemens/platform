module Games.Platformer exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div)
import Json.Decode as Decode
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { directon : Directon
    , characterPositionY : Int
    , characterPositionX : Int
    , itemPositionX : Int
    , itemPositionY : Int
    }


initialModel : Model
initialModel =
    { directon = Right
    , characterPositionX = 50
    , characterPositionY = 300
    , itemPositionX = 500
    , itemPositionY = 300
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = GameLoop Float
    | KeyDown String
    | NoOp
    | SetNewItemPositionX Int


type Directon
    = Right
    | Left


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            case key of
                "ArrowRight" ->
                    ( { model | directon = Right, characterPositionX = model.characterPositionX + 15 }, Cmd.none )

                "ArrowLeft" ->
                    ( { model | directon = Left, characterPositionX = model.characterPositionX - 15 }, Cmd.none )

                "ArrowDown" ->
                    ( { model | characterPositionY = model.characterPositionY + 15 }, Cmd.none )

                "ArrowUp" ->
                    ( { model | characterPositionY = model.characterPositionY - 15 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameLoop time ->
            if characterFoundItem model then
                ( model, Random.generate SetNewItemPositionX (Random.int 50 500) )

            else
                ( model, Cmd.none )

        -- ...
        NoOp ->
            ( model, Cmd.none )

        SetNewItemPositionX newPosX ->
            ( { model | itemPositionX = newPosX }, Cmd.none )


characterFoundItem : Model -> Bool
characterFoundItem model =
    let
        approximateItemLowerBound =
            model.itemPositionX - 35

        approximateItemUpperBound =
            model.itemPositionX

        approximateItemRange =
            List.range approximateItemLowerBound approximateItemUpperBound
    in
    List.member model.characterPositionX approximateItemRange



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onAnimationFrameDelta GameLoop
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewGame model ]


viewGame : Model -> Svg Msg
viewGame model =
    svg [ version "1.1", width "600", height "400" ]
        [ viewGameWindow
        , viewGameSky
        , viewGameGround
        , viewCharacter model
        , viewItem model
        ]


viewGameWindow : Svg Msg
viewGameWindow =
    rect
        [ width "600"
        , height "400"
        , fill "none"
        , stroke "black"
        ]
        []


viewGameSky : Svg Msg
viewGameSky =
    rect
        [ x "0"
        , y "0"
        , width "600"
        , height "300"
        , fill "#4b7cfb"
        ]
        []


viewGameGround : Svg Msg
viewGameGround =
    rect
        [ x "0"
        , y "300"
        , width "600"
        , height "100"
        , fill "green"
        ]
        []


viewCharacter : Model -> Svg Msg
viewCharacter model =
    image
        [ xlinkHref "/images/character.gif"
        , x (String.fromInt model.characterPositionX)
        , y (String.fromInt model.characterPositionY)
        , width "50"
        , height "50"
        ]
        []


viewItem : Model -> Svg Msg
viewItem model =
    if characterFoundItem model then
        svg [] []

    else
        image
            [ xlinkHref "/images/character.gif"
            , x (String.fromInt model.itemPositionX)
            , y (String.fromInt model.itemPositionY)
            , width "20"
            , height "20"
            ]
            []

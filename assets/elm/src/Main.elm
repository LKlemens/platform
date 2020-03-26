module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, h2, h3, img, li, p, strong, text, ul)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode



-- MAIN
-- main : Html


main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type alias Model =
    { gamesList : List Game
    , playersList : List Player
    }


type alias Player =
    { display_name : Maybe String
    , id : Int
    , score : Int
    , username : String
    }


type alias Game =
    { description : String
    , featured : Bool
    , slug : String
    , id : Int
    , thumbnail : String
    , title : String
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.batch
        [ fetchGamesList
        , fetchPlayerList
        ]


initialModel : Model
initialModel =
    { gamesList = []
    , playersList = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- API


fetchPlayerList : Cmd Msg
fetchPlayerList =
    Http.get
        { url = "/api/players"
        , expect = Http.expectJson FetchPlayersList decodePlayerList
        }


decodePlayer : Decode.Decoder Player
decodePlayer =
    Decode.map4 Player
        (Decode.maybe (Decode.field "display_name" Decode.string))
        (Decode.field "id" Decode.int)
        (Decode.field "score" Decode.int)
        (Decode.field "username" Decode.string)


decodePlayerList : Decode.Decoder (List Player)
decodePlayerList =
    decodePlayer
        |> Decode.list
        |> Decode.at [ "data" ]


fetchGamesList : Cmd Msg
fetchGamesList =
    Http.get
        { url = "/api/games"
        , expect = Http.expectJson FetchGamesList decodeGamesList
        }


decodeGame : Decode.Decoder Game
decodeGame =
    Decode.map6 Game
        (Decode.field "description" Decode.string)
        (Decode.field "featured" Decode.bool)
        (Decode.field "slug" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "thumbnail" Decode.string)
        (Decode.field "title" Decode.string)


decodeGamesList : Decode.Decoder (List Game)
decodeGamesList =
    decodeGame
        |> Decode.list
        |> Decode.at [ "data" ]



-- UPDATE


type Msg
    = FetchGamesList (Result Http.Error (List Game))
    | FetchPlayersList (Result Http.Error (List Player))



-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     ( updateModel msg model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGamesList result ->
            case result of
                Ok games ->
                    ( { model | gamesList = games }, Cmd.none )

                Err _ ->
                    Debug.log "Error fetching games from API."
                        ( model, Cmd.none )

        FetchPlayersList result ->
            case result of
                Ok players ->
                    ( { model | playersList = players }, Cmd.none )

                Err _ ->
                    Debug.log "Error fetching players from API."
                        ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ featured model
        , gamesIndex model
        , playersIndex model
        ]



-- featured : Model -> Html msg
-- featured model =
--     case featuredGame model.gamesList of
--         Just game ->
--             div [ class "row featured" ]
--                 [ div [ class "container" ]
--                     [ div [ class "featured-img" ]
--                         [ img [ class "featured-thumbnail", src game.thumbnail ] [] ]
--                     , div [ class "featured-data" ]
--                         [ h2 [] [ text "Featured" ]
--                         , h3 [] [ text game.title ]
--                         , p [] [ text game.description ]
--                         , button [ class "button" ] [ text "Play Now!" ]
--                         ]
--                     ]
--                 ]
--         Nothing ->
--             div [] []


featured : Model -> Html msg
featured model =
    case featuredGame model.gamesList of
        Just game ->
            div [ class "row featured" ]
                [ div [ class "container" ]
                    [ div [ class "featured-img" ]
                        [ img [ class "featured-thumbnail", src game.thumbnail ] [] ]
                    , div [ class "featured-data" ]
                        [ h2 [] [ text "Featured" ]
                        , h3 [] [ text game.title ]
                        , p [] [ text game.description ]
                        , a
                            [ class "button"
                            , href ("games/" ++ game.slug)
                            ]
                            [ text "Play Now!" ]
                        ]
                    ]
                ]

        Nothing ->
            div [] []


featuredGame : List Game -> Maybe Game
featuredGame games =
    games
        |> List.filter .featured
        |> List.head



-- div []
--     [ h1 [ class "game section" ] [ text "Games" ]
--     , gameIndex model
--     ]


playersIndex : Model -> Html msg
playersIndex model =
    let
        playersSortedByScore =
            model.playersList
                |> List.sortBy .score
                |> List.reverse
    in
    if List.isEmpty model.playersList then
        div [] []

    else
        div [ class "player-index container" ]
            [ h2 [] [ text "Players" ]
            , playersList playersSortedByScore
            ]


playersList : List Player -> Html msg
playersList players =
    ul [ class "player-list" ] (List.map playersListItem players)


playersListItem : Player -> Html msg
playersListItem player =
    let
        playerLink name =
            a [ href ("players/" ++ String.fromInt player.id) ]
                [ strong [ class "player-name" ] [ text name ] ]
    in
    li [ class "player-item" ]
        [ p [ class "player-score" ] [ text (String.fromInt player.score) ]
        , case player.display_name of
            Just display_name ->
                playerLink display_name

            Nothing ->
                playerLink player.username
        ]


gamesIndex : Model -> Html msg
gamesIndex model =
    if List.isEmpty model.gamesList then
        div [] []

    else
        div [ class "games-index container" ]
            [ h2 [] [ text "Games" ]
            , gamesList model.gamesList
            ]


gamesList : List Game -> Html msg
gamesList gameTitles =
    ul [ class "games-list" ] (List.map gamesListItem gameTitles)


gamesListItem : Game -> Html msg
gamesListItem game =
    a [ href ("games/" ++ game.slug) ]
        [ li [ class "game-item" ]
            [ div [ class "game-image" ]
                [ img [ src game.thumbnail ] []
                ]
            , div [ class "game-info" ]
                [ h3 [] [ text game.title ]
                , p [] [ text game.description ]
                ]
            ]
        ]



-- li [ class "game-item" ] [ strong [] [ text game.title ], p [] [ text game.description ] ]

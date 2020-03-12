module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra.Touch as Touch
import Keyboard
import Keyboard.Arrows
import Lamdera
import Task
import Time exposing (Posix)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = metaView
        }


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        KeyboardMsg keyMsg ->
            let
                newPressedKeys =
                    Keyboard.update keyMsg model.pressedKeys
            in
            ( model
            , Lamdera.sendToBackend
                (PressedKeys
                    newPressedKeys
                    (Keyboard.Arrows.arrows newPressedKeys
                        |> arrowsToPosition
                    )
                )
            )

        Resize size ->
            ( { model | size = size }, Cmd.none )

        StartMsg ( x, y ) ->
            ( { model | touch = ( x |> floor, y |> floor ) }, Cmd.none )

        EndMsg ( x, y ) ->
            let
                direction =
                    getDirection model.touch ( x |> floor, y |> floor )
            in
            ( { model | arrows = direction }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewModel snake apple ->
            ( { model | snake = snake, apple = apple }, Cmd.none )

        _ ->
            ( model, Cmd.none )


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


config =
    { fps = 60
    , tps = 5
    , max = 10
    , spriteSize = 20
    }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( Types.FrontendModel
        [ ( 0, 0 ) ]
        ( 0, 0 )
        ( 0, 0 )
        ( 1, 0 )
        []
        ( 0, 0 )
        ( 0, 0 )
        url
        key
    , Lamdera.sendToBackend ClientJoin
    )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        ]


metaView model =
    { title = "Not Only Meetings"
    , body =
        [ Html.div
            [ Touch.onStart (StartMsg << touchCoordinates)
            , Touch.onEnd (EndMsg << touchCoordinates)
            ]
            [ view model ]

        -- , WebGLViews.view model
        -- , asciiView model
        ]
    }


arrowsToPosition : Keyboard.Arrows.Arrows -> Position
arrowsToPosition arrows =
    ( arrows.x, arrows.y )


sign : Int -> Int
sign val =
    case val of
        0 ->
            0

        n ->
            ((n |> toFloat) / (abs n |> toFloat)) |> round


getDirection : Position -> Position -> Position
getDirection ( startX, startY ) ( endX, endY ) =
    let
        dx =
            endX - startX

        dy =
            endY - startY
    in
    if abs dx > abs dy then
        ( sign dx, 0 )

    else
        ( 0, -(sign dy) )


toPx val =
    (val |> String.fromInt) ++ "px"


wallsView =
    div
        [ style "width" ((config.max + 1) * config.spriteSize |> toPx)
        , style "height" ((config.max + 1) * config.spriteSize |> toPx)
        , style "background-color" "black"
        , style "position" "absolute"
        ]
        []


spriteView val ( x, y ) =
    div
        [ style "position" "absolute"
        , style "top" (y * config.spriteSize |> toPx)
        , style "left" (x * config.spriteSize |> toPx)
        ]
        [ text val ]


appleView : Position -> Html msg
appleView =
    spriteView "🍎"


vertebraView : Position -> Html msg
vertebraView =
    spriteView "🐍"


snakeView : List Position -> List (Html msg)
snakeView =
    List.map vertebraView


worldView : Model -> Html msg
worldView { apple, snake } =
    div [] (wallsView :: appleView apple :: snakeView snake)


scoreView : List a -> Html msg
scoreView snake =
    div
        [ style "position" "absolute"
        , style "color" "white"
        ]
        [ text ((List.length snake - 1) |> String.fromInt) ]


view : Model -> Html.Html FrontendMsg
view model =
    div [ style "user-select" "none" ]
        [ worldView model
        , scoreView model.snake
        ]

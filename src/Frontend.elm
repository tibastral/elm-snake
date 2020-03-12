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
import Random
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
            ( { model
                | pressedKeys = newPressedKeys
                , arrows = Keyboard.Arrows.arrows newPressedKeys |> arrowsToPosition
              }
            , Cmd.none
            )

        TickControl time ->
            ( model |> updateDirection, Cmd.none )

        Tick time ->
            model
                |> moveSnake

        NewApple newApple ->
            model
                |> addNewApple newApple

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
    ( model, Cmd.none )



-- view model =
--     { title = ""
--     , body =
--         [ Html.div [] [ Html.text model.message ]
--         ]
--     }
-- import Views
-- import WebGLViews
-- initialKeyboard : Keyboard.Model
-- initialKeyboard =
--     Tuple.first Keyboard.init


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
      --{ key = key, message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!" }
    , generateNewApple
    )



-- main : Program () Model Msg
-- main =
--     Browser.document
--         { init = \flags -> init
--         , update = update
--         , subscriptions = subscriptions
--         , view = metaView
--         }
-- renderDocument =
-- Html.program
-- { init = init
-- , view = metaView
-- , update = update
-- , subscriptions = subscriptions
-- }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        , Time.every (1000 / config.fps) TickControl
        , Time.every (1000 / config.tps) Tick

        -- , Browser.Events.onResize Resize
        ]


dropLastVertebra : List a -> List a
dropLastVertebra =
    List.reverse << List.drop 1 << List.reverse


calculateNewVertebra :
    Position
    -> Position
    -> Position
calculateNewVertebra ( x, y ) ( directionX, directionY ) =
    ( directionX + x
    , directionY + y
    )


addNewVertebra : Position -> List Position -> List Position
addNewVertebra direction snake =
    let
        currentHead =
            snake
                |> List.head
                |> Maybe.withDefault ( 0, 0 )

        newVertebra =
            calculateNewVertebra currentHead direction
    in
    newVertebra :: snake


collision : a -> List a -> Bool
collision =
    List.member


generateNewApple : Cmd FrontendMsg
generateNewApple =
    Random.generate NewApple
        (Random.pair
            (Random.int 1 config.max)
            (Random.int 1 config.max)
        )


between : comparable -> comparable -> comparable -> Bool
between minimum maximum value =
    value >= minimum && value <= maximum


out : comparable -> comparable -> ( comparable, comparable ) -> Bool
out minimum maximum ( x, y ) =
    let
        betweenBorders =
            between minimum maximum
    in
    (not <| betweenBorders y)
        || (not <| betweenBorders x)


collisionWithHimselfOrWall : List ( number, number ) -> Bool
collisionWithHimselfOrWall snake =
    case snake of
        head :: tail ->
            collision head tail || out 0 config.max head

        [] ->
            False


moveSnake : Model -> ( Model, Cmd FrontendMsg )
moveSnake ({ direction, snake, apple } as model) =
    let
        movedSnake =
            snake
                |> addNewVertebra direction

        appleEaten =
            collision apple movedSnake

        finalSnake =
            movedSnake
                |> (if appleEaten then
                        identity

                    else
                        dropLastVertebra
                   )
    in
    if collisionWithHimselfOrWall movedSnake then
        init model.url model.key

    else
        ( { model | snake = finalSnake }
        , if appleEaten then
            generateNewApple

          else
            Cmd.none
        )


applyKeyboard :
    ( Int, Int )
    -> ( Int, Int )
    -> ( Int, Int )
applyKeyboard ( arrowX, arrowY ) (( snakeDirectionX, snakeDirectionY ) as snakeDirection) =
    if arrowX /= 0 && snakeDirectionX == 0 then
        ( arrowX, 0 )

    else if arrowY /= 0 && snakeDirectionY == 0 then
        ( 0, -arrowY )

    else
        snakeDirection


updateDirection : Model -> Model
updateDirection model =
    let
        newDirection =
            model.direction
                |> applyKeyboard model.arrows
    in
    { model | direction = newDirection }


handleKeyboard : Keyboard.Msg -> Model -> ( Model, Cmd FrontendMsg )
handleKeyboard keyMsg model =
    -- KeyMsg keyMsg ->
    -- ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }
    -- , Cmd.none
    -- )
    -- case keyMsg of
    --     Keyboard.
    -- let
    -- ( keyboardModel, keyboardCmd ) =
    --     Keyboard.update keyMsg model.keyboardModel
    -- arrows =
    --     Keyboard.arrows keyboardModel
    -- newArrows : ( Int, Int )
    -- newArrows =
    --     ( arrows.x, arrows.y )
    -- in
    ( --{
      model
      -- | keyboardModel = keyboardModel
      -- , arrows = newArrows
      --   }
    , Cmd.none
      -- Cmd.map KeyboardMsg keyboardCmd
    )



-- asciiView model =
--     Html.pre []
--         [ Html.text ""
--         ]
--
-- metaView : Model -> Html.Html Msg


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


addNewApple : ( Int, Int ) -> Model -> ( Model, Cmd FrontendMsg )
addNewApple newApple model =
    if collision newApple model.snake then
        ( model, generateNewApple )

    else
        ( { model | apple = newApple }, Cmd.none )


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



-- view : Model -> Browser.Document FrontendMsg
-- view model =
--     { title = "v1"
--     , body = [ view model ]
--     }

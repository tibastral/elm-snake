module Main exposing (..)

import Time exposing (Time, second)
import Keyboard.Extra
import Random
import Html
import Types exposing (..)
import WebGLViews
import Views
import Window exposing (Size)
import Task


initialKeyboard : Keyboard.Extra.Model
initialKeyboard =
    Tuple.first Keyboard.Extra.init


init : ( Model, Cmd Msg )
init =
    ( Model
        [ ( 0, 0 ) ]
        ( 0, 0 )
        ( 0, 0 )
        ( 1, 0 )
        initialKeyboard
        (Size 0 0)
    , Cmd.batch
        [ generateNewApple
        , Task.perform Resize Window.size
        ]
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = metaView
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (second / config.fps) TickControl
        , Time.every (second / config.tps) Tick
        , Window.resizes Resize
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


generateNewApple : Cmd Msg
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


moveSnake : Model -> ( Model, Cmd Msg )
moveSnake ({ direction, snake, apple } as model) =
    let
        movedSnake =
            snake
                |> addNewVertebra direction

        appleEaten =
            collision apple movedSnake

        finalSnake =
            if appleEaten then
                movedSnake
            else
                movedSnake
                    |> dropLastVertebra
    in
        if collisionWithHimselfOrWall movedSnake then
            init
        else
            ( { model | snake = finalSnake }
            , if appleEaten then
                generateNewApple
              else
                Cmd.none
            )


applyKeyboard :
    ( number, number1 )
    -> ( number2, number3 )
    -> ( number2, number3 )
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


handleKeyboard :  Keyboard.Extra.Msg -> Model -> ( Model, Cmd Msg )
handleKeyboard  keyMsg model =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.update keyMsg model.keyboardModel

        arrows =
            Keyboard.Extra.arrows keyboardModel

        newArrows : ( Int, Int )
        newArrows =
            ( arrows.x, arrows.y )
    in
        ( { model
            | keyboardModel = keyboardModel
            , arrows = newArrows
          }
        , Cmd.map KeyboardMsg keyboardCmd
        )



-- asciiView model =
--     Html.pre []
--         [ Html.text ""
--         ]
--


metaView : Model -> Html.Html Msg
metaView model =
    Html.div []
        [ Views.view model
        , WebGLViews.view model

        -- , asciiView model
        ]


addNewApple : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
addNewApple newApple model =
    if collision newApple model.snake then
        ( model, generateNewApple )
    else
        ( { model | apple = newApple }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            handleKeyboard  keyMsg model

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

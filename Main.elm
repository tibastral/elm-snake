module Main exposing (..)

import Html
import Html.Attributes
import Time exposing (Time, second)
import Keyboard.Extra
import Random


type alias Position =
    ( Int, Int )


type alias Model =
    { snake : List Position
    , apple : Position
    , arrows : Position
    , direction : Position
    , keyboardModel : Keyboard.Extra.Model
    }


toPx val =
    (val |> toString) ++ "px"



-- snakeView snake =


draw val ( x, y ) =
    Html.div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "top", y * config.spriteSize |> toPx )
            , ( "left", x * config.spriteSize |> toPx )
            ]
        ]
        [ Html.text val ]


appleView =
    draw "🍎"


vertebraView =
    draw "🐍"


applesView apples =
    apples |> List.map appleView


snakeView snake =
    snake |> List.map vertebraView


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
    , generateNewApple
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Tick Time
    | TickControl Time
    | KeyboardMsg Keyboard.Extra.Msg
    | NewApple ( Int, Int )


config =
    { fps = 60
    , max = 10
    , spriteSize = 20
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (second / config.fps) TickControl
        , Time.every (second / 5) Tick
        ]


dropLastVertebra snake =
    snake
        |> List.reverse
        |> List.drop 1
        |> List.reverse


calculateNewVertebra ( x, y ) ( directionX, directionY ) =
    ( directionX + x
    , directionY + y
    )


addNewVertebra : ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
addNewVertebra direction snake =
    let
        currentHead =
            snake
                |> List.head
                |> Maybe.withDefault ( 0, 0 )

        newVertebra =
            calculateNewVertebra currentHead direction
    in
        (newVertebra :: snake)


collision apple snake =
    snake |> List.member apple


beEatenIfCollision : ( Int, Int ) -> List ( Int, Int ) -> Bool
beEatenIfCollision apple snake =
    snake |> collision apple


generateNewApple =
    Random.generate NewApple
        (Random.pair
            (Random.int 1 config.max)
            (Random.int 1 config.max)
        )


collisionWithHimself snake =
    case snake of
        e :: tail ->
            tail |> List.member e

        [] ->
            False


collisionWithWall snake =
    case snake of
        ( x, y ) :: tail ->
            x < 0 || x > config.max || y < 0 || y > config.max

        [] ->
            False


moveSnake : Model -> ( Model, Cmd Msg )
moveSnake ({ direction, snake, apple } as model) =
    let
        newSnake =
            snake
                |> addNewVertebra direction

        appleEaten =
            newSnake
                |> beEatenIfCollision apple

        finalSnake =
            if appleEaten then
                newSnake
            else
                newSnake |> dropLastVertebra
    in
        if collisionWithHimself newSnake || collisionWithWall newSnake then
            init
        else
            ( { model | snake = finalSnake }
            , if appleEaten then
                generateNewApple
              else
                Cmd.none
            )


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
            model.direction |> applyKeyboard model.arrows
    in
        { model | direction = newDirection }


handleKeyboard : Model -> Keyboard.Extra.Msg -> ( Model, Cmd Msg )
handleKeyboard model keyMsg =
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


addNewApple : ( Int, Int ) -> Model -> ( Model, Cmd Msg )
addNewApple newApple model =
    if model.snake |> collision newApple then
        ( model, generateNewApple )
    else
        ( { model | apple = newApple }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            handleKeyboard model keyMsg

        TickControl time ->
            ( model |> updateDirection, Cmd.none )

        Tick time ->
            model
                |> moveSnake

        NewApple newApple ->
            model |> addNewApple newApple


drawWalls =
    Html.div
        [ Html.Attributes.style
            [ ( "width", (config.max + 1) * config.spriteSize |> toPx )
            , ( "height", (config.max + 1) * config.spriteSize |> toPx )
            , ( "background-color", "black" )
            , ( "position", "absolute" )
            ]
        ]
        []


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div
            []
            (drawWalls :: (appleView model.apple) :: (snakeView model.snake))
        , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "color", "white" ) ] ]
            [ (Html.text ((List.length model.snake - 1) |> toString)) ]
        ]

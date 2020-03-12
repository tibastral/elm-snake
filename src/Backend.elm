module Backend exposing (..)

import Html
import Lamdera exposing (ClientId, SessionId)
import Random
import Set
import Time exposing (Posix)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


config =
    { fps = 60
    , tps = 5
    , max = 10
    , spriteSize = 20
    }


subscriptions model =
    Sub.batch
        [ Time.every (1000 / config.tps) Tick
        , Time.every (1000 / config.fps) TickControl
        ]


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


generateNewApple : Cmd BackendMsg
generateNewApple =
    Random.generate NewApple
        (Random.pair
            (Random.int 1 config.max)
            (Random.int 1 config.max)
        )


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


addNewApple : ( Int, Int ) -> Model -> ( Model, Cmd BackendMsg )
addNewApple newApple model =
    if collision newApple model.snake then
        ( model, generateNewApple )

    else
        ( { model | apple = newApple }, Cmd.none )


collisionWithHimselfOrWall : List ( number, number ) -> Bool
collisionWithHimselfOrWall snake =
    case snake of
        head :: tail ->
            collision head tail || out 0 config.max head

        [] ->
            False


moveSnake : Model -> ( Model, Cmd BackendMsg )
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
        initWithClients model.clients

    else
        ( { model | snake = finalSnake }
        , Cmd.batch
            ([ if appleEaten then
                generateNewApple

               else
                Cmd.none
             ]
                ++ [ broadcast model.clients (NewModel model.snake model.apple) ]
            )
        )


initWithClients : Set.Set ClientId -> ( Model, Cmd BackendMsg )
initWithClients clients =
    ( Types.BackendModel
        [ ( 0, 0 ) ]
        ( 0, 0 )
        ( 0, 0 )
        ( 1, 0 )
        []
        ( 0, 0 )
        ( 0, 0 )
        clients
    , Cmd.none
    )


init : ( Model, Cmd BackendMsg )
init =
    initWithClients Set.empty


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


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        Tick time ->
            model
                |> moveSnake

        TickControl time ->
            ( model |> updateDirection, Cmd.none )

        NewApple newApple ->
            model
                |> addNewApple newApple

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        ClientJoin ->
            ( { model | clients = Set.insert clientId model.clients }
            , Cmd.none
            )

        PressedKeys pressedKeys arrows ->
            ( { model | pressedKeys = pressedKeys, arrows = arrows }, Cmd.none )

        NoOpToBackend ->
            ( model, Cmd.none )


broadcast clients msg =
    clients
        |> Set.toList
        |> List.map (\clientId -> Lamdera.sendToFrontend clientId msg)
        |> Cmd.batch

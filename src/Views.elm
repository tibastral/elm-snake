module Views exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


toPx val =
    (val |> toString) ++ "px"


wallsView =
    div
        [ style
            [ ( "width", (config.max + 1) * config.spriteSize |> toPx )
            , ( "height", (config.max + 1) * config.spriteSize |> toPx )
            , ( "background-color", "black" )
            , ( "position", "absolute" )
            ]
        ]
        []


spriteView val ( x, y ) =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", y * config.spriteSize |> toPx )
            , ( "left", x * config.spriteSize |> toPx )
            ]
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
    div [] (wallsView :: (appleView apple) :: (snakeView snake))


scoreView : List a -> Html msg
scoreView snake =
    div [ style [ ( "position", "absolute" ), ( "color", "white" ) ] ]
        [ (text ((List.length snake - 1) |> toString)) ]


view : Model -> Html.Html Msg
view model =
    div []
        [ worldView model
        , scoreView model.snake
        ]

module Views exposing (..)

import Types exposing (..)
import Html
import Html.Attributes


toPx val =
    (val |> toString) ++ "px"


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


snakeView snake =
    snake |> List.map vertebraView


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div []
            (drawWalls :: (appleView model.apple) :: (snakeView model.snake))
        , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "color", "white" ) ] ]
            [ (Html.text ((List.length model.snake - 1) |> toString)) ]
        ]

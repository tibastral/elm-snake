module WebGLViews exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import WebGL exposing (Mesh, Shader)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul)


type alias Attributes =
    { position : Vec3 }


camera : Float -> Mat4
camera ratio =
    let
        c =
            (toFloat config.max + 1) / 2

        eye =
            vec3 c -c 15

        center =
            vec3 c c 0
    in
        mul (Math.Matrix4.makePerspective 45 ratio 0.01 100)
            (Math.Matrix4.makeLookAt eye center Math.Vector3.j)


cube : Mesh Attributes
cube =
    WebGL.triangles
        [ -- top
          ( Attributes (vec3 -0.5 0.5 -0.5), Attributes (vec3 -0.5 0.5 0.5), Attributes (vec3 0.5 0.5 0.5) )
        , ( Attributes (vec3 0.5 0.5 0.5), Attributes (vec3 0.5 0.5 -0.5), Attributes (vec3 -0.5 0.5 -0.5) )
          -- back
        , ( Attributes (vec3 -0.5 0.5 0.5), Attributes (vec3 0.5 0.5 0.5), Attributes (vec3 -0.5 -0.5 0.5) )
        , ( Attributes (vec3 -0.5 -0.5 0.5), Attributes (vec3 0.5 -0.5 0.5), Attributes (vec3 0.5 0.5 0.5) )
          -- right
        , ( Attributes (vec3 0.5 0.5 0.5), Attributes (vec3 0.5 -0.5 0.5), Attributes (vec3 0.5 -0.5 -0.5) )
        , ( Attributes (vec3 0.5 -0.5 -0.5), Attributes (vec3 0.5 0.5 -0.5), Attributes (vec3 0.5 0.5 0.5) )
          -- left
        , ( Attributes (vec3 -0.5 0.5 -0.5), Attributes (vec3 -0.5 0.5 0.5), Attributes (vec3 -0.5 -0.5 0.5) )
        , ( Attributes (vec3 -0.5 0.5 -0.5), Attributes (vec3 -0.5 -0.5 0.5), Attributes (vec3 -0.5 -0.5 -0.5) )
          -- front
        , ( Attributes (vec3 -0.5 0.5 -0.5), Attributes (vec3 0.5 0.5 -0.5), Attributes (vec3 0.5 -0.5 -0.5) )
        , ( Attributes (vec3 -0.5 0.5 -0.5), Attributes (vec3 0.5 -0.5 -0.5), Attributes (vec3 -0.5 -0.5 -0.5) )
          -- bottom
        , ( Attributes (vec3 -0.5 -0.5 -0.5), Attributes (vec3 -0.5 -0.5 0.5), Attributes (vec3 0.5 -0.5 0.5) )
        , ( Attributes (vec3 0.5 -0.5 0.5), Attributes (vec3 0.5 -0.5 -0.5), Attributes (vec3 -0.5 -0.5 -0.5) )
        ]


field : Mesh Attributes
field =
    let
        s =
            toFloat config.max + 1
    in
        WebGL.triangles
            [ ( Attributes (vec3 0 s -0.5), Attributes (vec3 s s -0.5), Attributes (vec3 0 0 -0.5) )
            , ( Attributes (vec3 0 0 -0.5), Attributes (vec3 s 0 -0.5), Attributes (vec3 s s -0.5) )
            ]


type alias Uniforms =
    { color : Vec3
    , offset : Vec3
    , camera : Mat4
    }


spriteView : Vec3 -> Float -> ( Int, Int ) -> WebGL.Entity
spriteView color ratio ( x, y ) =
    WebGL.entity
        vertexShader
        fragmentShader
        cube
        (Uniforms color (vec3 (toFloat x + 0.5) (toFloat (config.max - y) + 0.5) 0) (camera ratio))


appleView =
    spriteView (vec3 1 0 0)


wallsView ratio =
    WebGL.entity
        vertexShader
        fragmentShader
        field
        (Uniforms (vec3 0.4 0.2 0.3) (vec3 0 0 0) (camera ratio))


vertebraView =
    spriteView (vec3 0 1 0)


snakeView ratio snake =
    snake
        |> List.map (vertebraView ratio)


vertexShader : Shader Attributes Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform vec3 offset;
        uniform mat4 camera;
        void main () {
            gl_Position = camera * vec4(position + offset, 1.0);
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        void main () {
            gl_FragColor = vec4(color, 1.0);
        }
    |]


scoreView { snake } =
    div [ style [ ( "position", "relative" ), ( "text-align", "center" ), ( "font", "bold 30px/3 sans-serif" ) ] ]
        [ (text ((List.length snake - 1) |> toString)) ]


worldView : Model -> Html.Html Msg
worldView { apple, snake, size } =
    let
        ratio =
            (toFloat size.width / toFloat size.height)
    in
        WebGL.toHtml
            [ style
                [ ( "display", "block" )
                , ( "position", "absolute" )
                ]
            , width size.width
            , height size.height
            ]
            (wallsView ratio :: appleView ratio apple :: snakeView ratio snake)


view : Model -> Html.Html Msg
view model =
    div []
        [ worldView model
        , scoreView model
        ]

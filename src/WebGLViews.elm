module WebGLViews exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import WebGL exposing (Mesh, Shader)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4, mul)


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


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
            (Math.Matrix4.makeLookAt eye center Vec3.j)


attributes : Vec3 -> Vec3 -> Vec3 -> ( Attributes, Attributes, Attributes )
attributes p1 p2 p3 =
    let
        v1 =
            Vec3.sub p1 p2

        v2 =
            Vec3.sub p1 p3

        normal =
            Vec3.cross v1 v2 |> Vec3.normalize
    in
        ( Attributes p1 normal, Attributes p2 normal, Attributes p3 normal )


cube : Mesh Attributes
cube =
    WebGL.triangles
        [ -- back
          attributes (vec3 -0.5 0.5 -0.5) (vec3 -0.5 0.5 0.5) (vec3 0.5 0.5 0.5)
        , attributes (vec3 0.5 0.5 0.5) (vec3 0.5 0.5 -0.5) (vec3 -0.5 0.5 -0.5)
          -- top
        , attributes (vec3 -0.5 0.5 0.5) (vec3 -0.5 -0.5 0.5) (vec3 0.5 0.5 0.5)
        , attributes (vec3 -0.5 -0.5 0.5) (vec3 0.5 -0.5 0.5) (vec3 0.5 0.5 0.5)
          -- right
        , attributes (vec3 0.5 0.5 0.5) (vec3 0.5 -0.5 -0.5) (vec3 0.5 -0.5 0.5)
        , attributes (vec3 0.5 -0.5 -0.5) (vec3 0.5 0.5 0.5) (vec3 0.5 0.5 -0.5)
          -- left
        , attributes (vec3 -0.5 0.5 -0.5) (vec3 -0.5 0.5 0.5) (vec3 -0.5 -0.5 0.5)
        , attributes (vec3 -0.5 0.5 -0.5) (vec3 -0.5 -0.5 0.5) (vec3 -0.5 -0.5 -0.5)
          -- bottom
        , attributes (vec3 -0.5 0.5 -0.5) (vec3 0.5 0.5 -0.5) (vec3 0.5 -0.5 -0.5)
        , attributes (vec3 -0.5 0.5 -0.5) (vec3 0.5 -0.5 -0.5) (vec3 -0.5 -0.5 -0.5)
          -- front
        , attributes (vec3 -0.5 -0.5 -0.5) (vec3 0.5 -0.5 0.5) (vec3 -0.5 -0.5 0.5)
        , attributes (vec3 0.5 -0.5 0.5) (vec3 -0.5 -0.5 -0.5) (vec3 0.5 -0.5 -0.5)
        ]


field : Mesh Attributes
field =
    let
        s =
            toFloat config.max + 1
    in
        WebGL.triangles
            [ attributes (vec3 0 s -0.5) (vec3 0 0 -0.5) (vec3 s s -0.5)
            , attributes (vec3 0 0 -0.5) (vec3 s 0 -0.5) (vec3 s s -0.5)
            ]


sphere : Mesh Attributes
sphere =
    divideSphere 5 octahedron
        |> List.map (\( p1, p2, p3 ) -> attributes p1 p2 p3)
        |> WebGL.triangles


{-| Recursively divide an octahedron to turn it into a sphere
-}
divideSphere : Int -> List ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divideSphere step triangles =
    if step == 0 then
        triangles
    else
        divideSphere (step - 1) (List.concatMap divide triangles)


{-|
        1
       / \
    b /___\ c
     /\   /\
    /__\ /__\
   0    a    2
-}
divide : ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divide ( v0, v1, v2 ) =
    let
        a =
            Vec3.add v0 v2 |> Vec3.normalize |> Vec3.scale 0.5

        b =
            Vec3.add v0 v1 |> Vec3.normalize |> Vec3.scale 0.5

        c =
            Vec3.add v1 v2 |> Vec3.normalize |> Vec3.scale 0.5
    in
        [ ( v0, b, a ), ( b, v1, c ), ( a, b, c ), ( a, c, v2 ) ]


{-| Octahedron
-}
octahedron : List ( Vec3, Vec3, Vec3 )
octahedron =
    [ ( vec3 0.5 0 0, vec3 0 0.5 0, vec3 0 0 0.5 )
    , ( vec3 0 0.5 0, vec3 -0.5 0 0, vec3 0 0 0.5 )
    , ( vec3 -0.5 0 0, vec3 0 -0.5 0, vec3 0 0 0.5 )
    , ( vec3 0 -0.5 0, vec3 0.5 0 0, vec3 0 0 0.5 )
    , ( vec3 0.5 0 0, vec3 0 0 -0.5, vec3 0 0.5 0 )
    , ( vec3 0 0.5 0, vec3 0 0 -0.5, vec3 -0.5 0 0 )
    , ( vec3 -0.5 0 0, vec3 0 0 -0.5, vec3 0 -0.5 0 )
    , ( vec3 0 -0.5 0, vec3 0 0 -0.5, vec3 0.5 0 0 )
    ]


type alias Uniforms =
    { color : Vec3
    , offset : Vec3
    , camera : Mat4
    }


vertebraView : Float -> ( Int, Int ) -> WebGL.Entity
vertebraView ratio ( x, y ) =
    WebGL.entity
        vertexShader
        fragmentShader
        cube
        (Uniforms (vec3 0 1 0) (vec3 (toFloat x + 0.5) (toFloat (config.max - y) + 0.5) 0) (camera ratio))


appleView ratio ( x, y ) =
    WebGL.entity
        vertexShader
        fragmentShader
        sphere
        (Uniforms (vec3 1 0 0) (vec3 (toFloat x + 0.5) (toFloat (config.max - y) + 0.5) 0) (camera ratio))


wallsView ratio =
    WebGL.entity
        vertexShader
        fragmentShader
        field
        (Uniforms (vec3 0.4 0.2 0.3) (vec3 0 0 0) (camera ratio))


snakeView ratio snake =
    snake
        |> List.map (vertebraView ratio)


type alias Varying =
    { vlighting : Float
    }


vertexShader : Shader Attributes Uniforms Varying
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform vec3 offset;
        uniform mat4 camera;
        varying highp float vlighting;
        void main () {
            highp float ambientLight = 0.4;
            highp float directionalLight = 0.6;
            highp vec3 directionalVector = normalize(vec3(0, -2, 3));
            gl_Position = camera * vec4(position + offset, 1.0);
            highp float directional = max(dot(normal, directionalVector), 0.0);
            vlighting = ambientLight + directional * directionalLight;
        }
    |]


fragmentShader : Shader {} Uniforms Varying
fragmentShader =
    [glsl|
        precision mediump float;
        varying highp float vlighting;
        uniform vec3 color;
        void main () {
            gl_FragColor = vec4(color * vlighting, 1.0);
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

module HyperbolicPlaneTree exposing (run, Model, Msg, Vertex)

import Html
import Html.Attributes
import Svg
import Svg.Attributes

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture, Error, loadWith, nonPowerOfTwoOptions)

import Tree exposing (Tree)
import TreeZipper exposing (TreeZipper)
import Lazy exposing (Lazy, lazy, force)
import List.Extra as List

import Keyboard




type alias Program =
    { init : Model
    , update : Msg -> Model -> Model
    , view : Texture -> Model -> Html.Html Msg
    , subscriptions : Sub Msg
    } 


type alias Model =
    { r : Float
    , theta : Float
    , zipper : TreeZipper (Mesh Vertex)
    }

type Msg
    = Key Keyboard.KeyCode


run : Tree (Mesh Vertex) -> Program
run theTree =
    { init = {r = 0, theta = 0, zipper = TreeZipper.fromTree theTree}
    , update =
        let
            handleMsg msg model = case msg of
                Key 65 -> {model | theta = model.theta - 0.05 * e ^ model.r}
                Key 68 -> {model | theta = model.theta + 0.05 * e ^ model.r}
                Key 87 -> {model | r = model.r - 0.05}
                Key 83 -> {model | r = model.r + 0.05}
                Key _ -> model

            moveZipper oldmodel model = 
                if model.r > 0 || abs model.theta > 0.5
                then case TreeZipper.up model.zipper of
                    Err _ -> oldmodel
                    Ok (m,z) -> 
                        let n = toFloat (List.length (TreeZipper.down z))
                        in moveZipper oldmodel {model | zipper = z, theta = (model.theta + 1/2 + (n-1-toFloat m)) / n - 1/2, r = model.r - logBase e n }
                else let n = List.length (TreeZipper.down model.zipper)
                in if e ^ -model.r > toFloat n
                then 
                    let a = (model.theta + 1/2) * toFloat n
                        b = floor a
                        c = a - toFloat b - 1/2
                    in case List.getAt (n-1-b) (TreeZipper.down model.zipper) of
                        Nothing -> model
                        Just newzipper ->
                            moveZipper oldmodel
                                { model 
                                | zipper = newzipper
                                , r = model.r + logBase e (toFloat n)
                                , theta = c
                                }
                            
                else model
        in \msg model -> moveZipper model (handleMsg msg model)
    , view = \tx {r, theta, zipper} ->
        Html.div
        [ Html.Attributes.width 1500
        , Html.Attributes.height 1500
        , Html.Attributes.style [ ("position", "relative") ]
        ]
        [ Svg.svg
            [ Html.Attributes.width 1500
            , Html.Attributes.height 1500
            , Html.Attributes.style [ ("position", "absolute") ] 
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "750"
                , Svg.Attributes.cy "750"
                , Svg.Attributes.r "750"
                , Svg.Attributes.fill "#C0C0C0"
                ]
                []
            ]
        , WebGL.toHtmlWith
            [ WebGL.alpha True
            , WebGL.antialias
            , WebGL.depth 1
            , WebGL.stencil 0
            ]
            [ Html.Attributes.width 1500
            , Html.Attributes.height 1500
            , Html.Attributes.style [ ( "display", "block" ), ("position", "absolute") ]
            ]
            (renderTree
                (\mat mesh ->
                    WebGL.entity
                        vertexShader
                        fragmentShader
                        mesh
                        { texture = tx
                        , perspective =
                            List.foldr Mat4.mul Mat4.identity
                                [ Mat4.makePerspective 90 1 1 100
                                , Mat4.makeLookAt (vec3 0 0 -1) (vec3 0 0 0) (vec3 0 1 0)
                                , mat1 r
                                , mat2 theta
                                , mat
                                ]
                        })
                (Tree.take 1 (TreeZipper.belowCursor zipper))
                )
        ]
    , subscriptions = Keyboard.downs Key
    }





renderTree : (Mat4 -> a -> b) -> Tree a -> List b
renderTree f =
    let help : List b -> Float -> Float -> Tree.TreeView a -> List b
        help acc r theta (Tree.Cons a forest) =
            let transform = Mat4.mul (mat1 r) (mat2 theta)
                newacc = f transform a :: acc
                lf = toFloat (List.length (force forest))
                newr = r + logBase e lf
            in List.foldl
                (\t (a,x) ->
                    (help a newr x (force t), x+1))
                (newacc, (theta - 1/2) * lf + 1/2)
                (force forest)
                |> Tuple.first
    in help [] 0 0 << force



type alias Vertex =
    { position : Vec3
    , coord : Vec2
    , color : Vec3
    }

type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
    }


vertexShader : Shader Vertex { u | perspective : Mat4 } { vcoord : Vec2, vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec2 vcoord;
        varying vec3 vcolor;
        void main () {
          gl_Position = perspective * vec4(position, 1.0);
          vcoord = coord;
          vcolor = color;
        }
    |]

fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2, vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec3 vcolor;
        varying vec2 vcoord;
        void main () {
          vec4 texcol = texture2D(texture, vcoord);
          gl_FragColor = mix(vec4(vcolor, 1.0), texcol, texcol.a);
        }
    |]

-- Simple rotation in Minkowski space
mat1 : Float -> Mat4
mat1 x = 
    Mat4.makeBasis
        (vec3 1 0 0)
        (vec3 0 (cosh x) (sinh x))
        (vec3 0 (sinh x) (cosh x))

-- Magic. (actually obtained from taking the limit of the rotation formula as the angle goes to zero and the axis goes to a null vector.)
mat2 : Float -> Mat4
mat2 x = 
    Mat4.makeBasis
        (vec3 1 -x x)
        (vec3 x (1-x^2/2) (0+x^2/2))
        (vec3 x (0-x^2/2) (1+x^2/2))

sinh : Float -> Float
sinh x = (e ^ x - e ^ -x) / 2
cosh : Float -> Float
cosh x = (e ^ x + e ^ -x) / 2


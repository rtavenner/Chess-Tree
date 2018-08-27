import Html
import Html.Lazy
import Task

import Tree exposing (Tree)

import Dict exposing (Dict)
import Set exposing (Set)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture, Error, loadWith, nonPowerOfTwoOptions)

import Keyboard

import Chess exposing (Color(..), Board, Piece(..))

import HyperbolicPlaneTree exposing (Vertex)

import AnimationFrame
import Time exposing (Time)


type Model
    = LoadingAssets
    | LoadingFailed Error
    | MainApp Texture HyperbolicPlaneTree.Model

type Msg
    = TextureLoaded (Result Error Texture)
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | Tick Time

main : Program Never {keys : Set Keyboard.KeyCode, model : Model} Msg
main = 
    let {init, update, view} = HyperbolicPlaneTree.run (Tree.map (WebGL.triangles << chessBoard) Chess.theTree)
    in Html.program
        { init = 
            ( { model = LoadingAssets
              , keys = Set.empty
              }
            , Task.attempt TextureLoaded (loadWith nonPowerOfTwoOptions "./Pieces.png")
            )
        , update = \msg m ->
            case (msg,m.model) of
                (TextureLoaded (Ok tx), _) -> ({m | model = MainApp tx init}, Cmd.none)
                (Tick dt, MainApp tx model) -> 
                    let r = (if Set.member 87 m.keys then (Time.inSeconds dt) else 0) - (if Set.member 83 m.keys then (Time.inSeconds dt) else 0)
                        t = (if Set.member 68 m.keys then (Time.inSeconds dt) else 0) - (if Set.member 65 m.keys then (Time.inSeconds dt) else 0)
                    in if r == 0 && t == 0
                    then (m, Cmd.none)
                    else
                        ( { m
                          | model = MainApp tx (update {r = r, theta = t} model)
                          }
                        , Cmd.none
                        )
                (KeyDown k, _) -> ({m | keys = Set.insert k m.keys}, Cmd.none)
                (KeyUp   k, _) -> ({m | keys = Set.remove k m.keys}, Cmd.none)
                _ -> (m, Cmd.none)
        , view = Html.Lazy.lazy (\{model} -> case model of
            LoadingAssets -> 
                Html.text "Loading..."
            LoadingFailed err -> 
                Html.div
                    []
                    [ Html.text "Failed to load assets. The error was:"
                    , Html.text (toString err)
                    ]
            MainApp tx model -> Html.map never (view tx model))
        , subscriptions = \_ -> Sub.batch
            [ Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            , AnimationFrame.diffs Tick
            ]
        }





chessCoords : List (Int,Int)
chessCoords = List.range 0 7 |> List.concatMap (\x -> List.range 0 7 |> List.map (\y -> (x,y)))

chessBoard : Board -> List (Vertex, Vertex, Vertex)
chessBoard board =
    let r = vec3 0.035 0 0
        u = vec3 0 0.035 0
    in List.concatMap 
        (\(x,y) -> chessPiece
            (if (x+y) % 2 == 0 then vec3 0 0 0 else vec3 1 1 1)
            (Dict.get (x,y) board)
            (r,u)
            (Vec3.add
                (Vec3.add
                    (Vec3.scale (7 - 2 * toFloat x) r) 
                    (Vec3.scale (2 * toFloat y - 7) u)) 
                (vec3 0 0 1)))
        chessCoords
        |> List.map 
            ((\f (a,b,c) -> (f a, f b, f c)) 
                (\vertex ->
                    { vertex
                    | position = 
                        let (x,y,z) = Vec3.toTuple vertex.position
                        in Vec3.scale (1 / sqrt (z*z-x*x-y*y)) vertex.position
                    }))

chessPiece : Vec3 -> Maybe (Color,Piece) -> (Vec3,Vec3) -> Vec3 -> List (Vertex, Vertex, Vertex)
chessPiece = 
    let f u v =
            vec2
                ((208 * toFloat u + 16) / 1280)
                ((335 * toFloat v + 14) / 698)
    in \color piece (right,up) center ->
        let (u,v) = case piece of
                Nothing -> (-1,0)
                Just (Black, Pawn)   -> (5,0)
                Just (Black, Rook _) -> (4,0)
                Just (Black, Knight) -> (3,0)
                Just (Black, Bishop) -> (2,0)
                Just (Black, Queen)  -> (1,0)
                Just (Black, King _) -> (0,0)
                Just (White, Pawn)   -> (0,1)
                Just (White, Rook _) -> (1,1)
                Just (White, Knight) -> (2,1)
                Just (White, Bishop) -> (3,1)
                Just (White, Queen)  -> (4,1)
                Just (White, King _) -> (5,1)
        in
            [ ( { position = center |> flip Vec3.add right |> flip Vec3.sub up
                , coord = f (u+0) (v+0)
                , color = color
                }
              , { position = center |> flip Vec3.sub right |> flip Vec3.sub up
                , coord = f (u+1) (v+0)
                , color = color
                }
              , { position = center |> flip Vec3.add right |> flip Vec3.add up
                , coord = f (u+0) (v+1)
                , color = color
                }
              )
            , ( { position = center |> flip Vec3.sub right |> flip Vec3.add up
                , coord = f (u+1) (v+1)
                , color = color
                }
              , { position = center |> flip Vec3.sub right |> flip Vec3.sub up
                , coord = f (u+1) (v+0)
                , color = color
                }
              , { position = center |> flip Vec3.add right |> flip Vec3.add up
                , coord = f (u+0) (v+1)
                , color = color
                }
              )
            ]

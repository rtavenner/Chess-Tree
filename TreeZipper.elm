module TreeZipper exposing (TreeZipper, up, down, toTree, fromTree, belowCursor)

import Tree exposing (Tree, Forest)
import ListZipper exposing (ListZipper, ListCtx)
import Lazy exposing (Lazy, lazy, force)

type alias TreeZipper a = (a, TreeCtx a)
type TreeCtx a = TreeCtx (List (a, ListCtx (Tree a))) (Forest a)

up : TreeZipper a -> Result (Tree a) (Int,TreeZipper a)
up (x, TreeCtx above below) =
    case above of
        [] -> Err (Tree.cons x below)
        (a,y)::cs -> 
            let (n,f) = ListZipper.toList (Tree.cons x below, y)
            in Ok (n, (a, TreeCtx cs (lazy <| \() -> f)))
        --Ok (a, TreeCtx cs (ListZipper.toList (Tree.cons x below, y)))

down : TreeZipper a -> List (TreeZipper a)
down (x, TreeCtx above below) =
    ListZipper.fromList (force below)
    |> List.map (\(t,ctx) -> (Tree.head t, TreeCtx ((x,ctx)::above) (Tree.tail t)))

toTree : TreeZipper a -> Tree a
toTree z =
    case up z of
        Err t -> t
        Ok (_,z2) -> toTree z2

fromTree : Tree a -> TreeZipper a
fromTree t = (Tree.head t, TreeCtx [] (Tree.tail t))


belowCursor : TreeZipper a -> Tree a
belowCursor (x, TreeCtx _ xs) = Tree.cons x xs

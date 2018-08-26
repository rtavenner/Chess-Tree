module Tree exposing (..)

import Lazy exposing (Lazy, lazy, force)

type alias Tree a = Lazy (TreeView a)
type TreeView a = Cons a (Forest a)
type alias Forest a = Lazy (List (Tree a))


head : Tree x -> x
head t =
    case force t of
        Cons x _ -> x

tail : Tree x -> Forest x
tail t =
    case force t of
        Cons _ xs -> xs

cons : x -> Forest x -> Tree x
cons x xs = lazy <| \() -> Cons x xs

unfold : (b -> a) -> (b -> List b) -> b -> Tree a
unfold f g b = lazy <| \() -> Cons (f b) (lazy <| \() -> List.map (unfold f g) (g b))

fold : ((a,List b) -> b) -> Tree a -> b
fold f t =
    case force t of
        Cons x xs -> f (x, List.map (fold f) (force xs))

take : Int -> Tree x -> Tree x
take n t =
    case force t of
        Cons x xs -> lazy <| \() -> Cons x (if n <= 0 then lazy <| \() -> [] else Lazy.map (List.map (take (n-1))) xs)


map : (a -> b) -> (Tree a -> Tree b)
map f =
    Lazy.map (\t ->
        case t of
            Cons x xs -> Cons (f x) (Lazy.map (List.map (map f)) xs))
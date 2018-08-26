module ListZipper exposing (..)

import List.Extra as List

type alias ListZipper a = (a, ListCtx a)
type ListCtx a = ListCtx (List a) (List a)

up : ListZipper a -> Result (List a) (ListZipper a)
up (x, ListCtx above below) =
    case above of
        [] -> Err (x::below)
        y::ys -> Ok (y, ListCtx ys (x::below))

down : ListZipper a -> Maybe (ListZipper a)
down (x, ListCtx above below) =
    case below of
        [] -> Nothing
        y::ys -> Just (y, ListCtx (x::above) ys)

toList : ListZipper a -> (Int, List a)
toList (x, ListCtx above below) = (List.length above, List.reverse above ++ x :: below)

fromList : List a -> List (ListZipper a)
fromList = List.selectSplit >> List.map (\(above,x,below) -> (x, ListCtx (List.reverse above) below))


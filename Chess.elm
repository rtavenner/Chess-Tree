module Chess exposing (theTree, Color(..), Piece(..), Board)

import Dict exposing (Dict)

import Tree


type alias GameState =
    { state : State
    , prevStates : List State -- since last capture or pawn move
    }
type alias State = 
    { board : Board
    , turn : Color
    , enPassantPossibleAt : Maybe Int
    }
type alias Board = Dict Loc (Color,Piece)

type alias Loc = (Int,Int)

type Color = Black | White
otherColor : Color -> Color
otherColor color = case color of
    Black -> White
    White -> Black

type Piece
    = Pawn
    | Rook {hasMoved : Bool}
    | Knight
    | Bishop
    | Queen
    | King {hasMoved : Bool}




initialBoard : Board
initialBoard =
    Dict.fromList
        [ ((4,0), (White, King {hasMoved = False}))
        , ((4,7), (Black, King {hasMoved = False}))

        , ((0,0), (White, Rook {hasMoved = False}))
        , ((1,0), (White, Knight))
        , ((2,0), (White, Bishop))
        , ((3,0), (White, Queen))
        , ((5,0), (White, Bishop))
        , ((6,0), (White, Knight))
        , ((7,0), (White, Rook {hasMoved = False}))
        , ((0,1), (White, Pawn))
        , ((1,1), (White, Pawn))
        , ((2,1), (White, Pawn))
        , ((3,1), (White, Pawn))
        , ((4,1), (White, Pawn))
        , ((5,1), (White, Pawn))
        , ((6,1), (White, Pawn))
        , ((7,1), (White, Pawn))
        , ((0,6), (Black, Pawn))
        , ((1,6), (Black, Pawn))
        , ((2,6), (Black, Pawn))
        , ((3,6), (Black, Pawn))
        , ((4,6), (Black, Pawn))
        , ((5,6), (Black, Pawn))
        , ((6,6), (Black, Pawn))
        , ((7,6), (Black, Pawn))
        , ((0,7), (Black, Rook {hasMoved = False}))
        , ((1,7), (Black, Knight))
        , ((2,7), (Black, Bishop))
        , ((3,7), (Black, Queen))
        , ((5,7), (Black, Bishop))
        , ((6,7), (Black, Knight))
        , ((7,7), (Black, Rook {hasMoved = False}))
        ]

initialState : State
initialState =
    { board = initialBoard
    , turn = White
    , enPassantPossibleAt = Nothing
    }

initialGameState : GameState
initialGameState =
    { state = initialState
    , prevStates = []
    }




theTree : Tree.Tree Board 
theTree = 
    Tree.unfold 
        (\gameState -> gameState.state.board)
        (\gameState ->
            List.filterMap
                (\g ->
                    if inCheck (gameState.state.turn) g.state.board
                    then Nothing
                    else Just g)
                (calcNextGameStates gameState))
        initialGameState


hasDeadKing : Board -> Bool
hasDeadKing board = 
    let pieces = Dict.values board
        kings = 
            List.filterMap 
                (\(color, piece) -> case piece of
                    King _ -> Just color
                    _ -> Nothing)
                pieces
    in not (List.member White kings && List.member Black kings)


calcNextGameStates : GameState -> List GameState
calcNextGameStates gameState = 
    if List.length gameState.prevStates >= 100
    then []  -- Fifty move rule: This is a draw.
    else if List.length (List.filter ((==) gameState.state) gameState.prevStates) >= 2
    then []  -- Three repitition rule: This is a draw.
    else
        List.map
            (\{board, isCapture, pawnMove} ->
                { state = 
                    { board = board
                    , turn = otherColor gameState.state.turn
                    , enPassantPossibleAt =
                        case pawnMove of 
                            PawnMoveTwo x -> Just x
                            _ -> Nothing
                    }
                , prevStates = if (pawnMove /= NotAPawnMove) || isCapture then [] else gameState.state :: gameState.prevStates
                })
            (calcMoves gameState.state)

calcMoves : State -> List { board : Board, isCapture : Bool, pawnMove : PawnMove }
calcMoves state =
    List.concatMap
        (calcMovesForPiece state)
        (Dict.toList state.board)


calcMovesForPiece : State -> (Loc, (Color,Piece)) -> List { board : Board, isCapture : Bool, pawnMove : PawnMove }
calcMovesForPiece state ((x,y) as loc, (color,piece)) =
    if color /= state.turn
    then []
    else case piece of
        Pawn -> 
            let fwd = if color == White then \(x,y) -> (x,y+1) else \(x,y) -> (x,y-1) 
                baseLine = if color == White then 1 else 6
                enPassantLine = if color == White then 4 else 3
            in 
            List.concatMap 
                (List.concatMap (\(l,c) -> 
                    if List.member (Tuple.second l) [0,7]
                    then 
                        [ {board = Dict.insert l (color, Rook {hasMoved = True}) (Dict.remove loc state.board), isCapture = c, pawnMove = PawnMove}
                        , {board = Dict.insert l (color, Knight                ) (Dict.remove loc state.board), isCapture = c, pawnMove = PawnMove}
                        , {board = Dict.insert l (color, Bishop                ) (Dict.remove loc state.board), isCapture = c, pawnMove = PawnMove}
                        , {board = Dict.insert l (color, Queen                 ) (Dict.remove loc state.board), isCapture = c, pawnMove = PawnMove}
                        ]
                    else [{board = move loc l state.board, isCapture = c, pawnMove = PawnMove}]))
                [ case get (fwd loc) state.board of
                    Just Nothing -> [(fwd loc, False)]
                    _ -> []
                , case get (fwd (x+1,y)) state.board of
                    Just (Just (col,_)) -> 
                        if col /= color
                        then [(fwd (x+1,y), True)]
                        else []
                    _ -> []
                , case get (fwd (x-1,y)) state.board of
                    Just (Just (col,_)) -> 
                        if col /= color
                        then [(fwd (x-1,y), True)]
                        else []
                    _ -> []
                ]
            ++ List.filterMap identity
                [ if y == baseLine
                    then case get (fwd loc) state.board of
                        Just Nothing -> case get (fwd (fwd loc)) state.board of
                            Just Nothing -> 
                                Just {board = move loc (fwd (fwd loc)) state.board, isCapture = False, pawnMove = PawnMoveTwo x}
                            _ -> Nothing
                        _ -> Nothing
                    else Nothing
                , case state.enPassantPossibleAt of
                    Nothing -> Nothing
                    Just xEP -> 
                        if y == enPassantLine && abs (x - xEP) == 1 
                        then Just
                            { board = 
                                state.board
                                |> Dict.remove (xEP, enPassantLine)
                                |> move loc (fwd (xEP, enPassantLine))
                            , isCapture = True
                            , pawnMove = PawnMove
                            }
                        else Nothing
                ]
        Rook _ -> 
            List.concatMap (\dir -> zoom color dir loc state.board)
                [ \(x,y) -> (x+1,y)
                , \(x,y) -> (x-1,y)
                , \(x,y) -> (x,y+1)
                , \(x,y) -> (x,y-1)
                ]
        Knight -> 
            List.filterMap
                (\(dx,dy) -> stomp NotAPawnMove color loc (x+dx, y+dy) state.board)
                [ ( 1, 2)
                , ( 2, 1)
                , ( 2,-1)
                , ( 1,-2)
                , (-1,-2)
                , (-2,-1)
                , (-2, 1)
                , (-1, 2)
                ]
        Bishop -> 
            List.concatMap (\dir -> zoom color dir loc state.board)
                [ \(x,y) -> (x+1,y+1)
                , \(x,y) -> (x+1,y-1)
                , \(x,y) -> (x-1,y+1)
                , \(x,y) -> (x-1,y-1)
                ]
        Queen  ->
            List.concatMap (\dir -> zoom color dir loc state.board)
                [ \(x,y) -> (x+1,y  )
                , \(x,y) -> (x+1,y+1)
                , \(x,y) -> (x  ,y+1)
                , \(x,y) -> (x-1,y+1)
                , \(x,y) -> (x-1,y  )
                , \(x,y) -> (x-1,y-1)
                , \(x,y) -> (x  ,y-1)
                , \(x,y) -> (x+1,y-1)
                ]
        King {hasMoved} ->
            ( if hasMoved then []
                else List.filterMap identity 
                    [ attemptCastle color (\(x,y) -> (x+1,y)) loc state.board
                    , attemptCastle color (\(x,y) -> (x-1,y)) loc state.board
                    ])
            ++ List.filterMap
                (\(dx,dy) -> stomp NotAPawnMove color loc (x+dx, y+dy) state.board)
                [ (-1,-1)
                , (-1, 0)
                , (-1, 1)
                , ( 0,-1)
                , ( 0, 1)
                , ( 1,-1)
                , ( 1, 0)
                , ( 1, 1)
                ]

move : Loc -> Loc -> Board -> Board
move oldloc newloc board =
    case Dict.get oldloc board of
        Nothing -> Dict.remove newloc board
        Just x -> 
            let x_ = case x of
                    (c,Rook _) -> (c,Rook {hasMoved = True})
                    (c,King _) -> (c,King {hasMoved = True})
                    _ -> x
            in Dict.insert newloc x_ (Dict.remove oldloc board)

stomp : PawnMove -> Color -> Loc -> Loc -> Board -> Maybe { board : Board, isCapture : Bool, pawnMove : PawnMove }
stomp pm color oldloc newloc board =
    case get newloc board of
        Just Nothing -> 
            Just {board = move oldloc newloc board, isCapture = False, pawnMove = pm}
        Just (Just (col,_)) -> 
            if col /= color
            then Just {board = move oldloc newloc board, isCapture = True, pawnMove = pm}
            else Nothing 
        _ -> Nothing

zoom : Color -> (Loc -> Loc) -> Loc -> Board -> List { board : Board, isCapture : Bool, pawnMove : PawnMove }
zoom color direction loc board =
    case stomp NotAPawnMove color loc (direction loc) board of
        Nothing -> []
        Just ({board, isCapture} as info) ->
            info :: if isCapture then [] else zoom color direction (direction loc) board

-- Castling rules:
--    If king or rook has moved, fail.
--    If squares between king and rook aren't all empty, fail.
--    If king is in check, fail.
--    Move king one square toward rook.
--    If king is in check, fail.
--    Move king second square toward rook.
--    If king is in check, fail. (This is caught by check for check at end of move, so does not need to be coded.)
--    Move rook to other side.
attemptCastle : Color -> (Loc -> Loc) -> Loc -> Board -> Maybe { board : Board, isCapture : Bool, pawnMove : PawnMove }
attemptCastle color direction kingLoc board = 
    let findRook loc = 
            case get (direction loc) board of
                Just Nothing -> findRook (direction loc)
                Just (Just (col,Rook {hasMoved})) ->
                    if not hasMoved && col == color
                    then Just (direction loc)
                    else Nothing
                _ -> Nothing
    in if inCheck color board
    then Nothing
    else if inCheck color (move kingLoc (direction kingLoc) board)
    then Nothing
    else
        findRook kingLoc |> Maybe.map (\rookLoc ->
            { board = 
                board
                |> move kingLoc (direction (direction kingLoc))
                |> move rookLoc (direction kingLoc)
            , isCapture = False
            , pawnMove = NotAPawnMove
            })

inCheck : Color -> Board -> Bool
inCheck color board =
    List.any (.board >> hasDeadKing)
        (calcMoves
            { board = 
                Dict.map  -- HACKY: Disallow castling to avoid infinite recursion
                    (\_ ((col,piece) as x) ->
                        case piece of
                            King _ -> (col, King {hasMoved = True})
                            _ -> x)
                    board
            , turn = otherColor color
            , enPassantPossibleAt = Nothing
            })


get : Loc -> Board -> Maybe (Maybe (Color, Piece))
get ((x,y) as loc) board =
    if x < 0 || y < 0 || x > 7 || y > 7
    then Nothing -- Not on board
    else Just (Dict.get loc board) -- On board, possibly empty



type PawnMove
    = NotAPawnMove
    | PawnMove
    | PawnMoveTwo Int -- Specially tracked because En Passant may be possible!




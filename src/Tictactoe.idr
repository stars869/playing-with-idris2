module Tictactoe

import Vector
import Matrix 
import Finite 

public export 
data Mark : Type where 
    X : Mark
    O : Mark 
    Empty : Mark 

Eq Mark where  
    X == X = True 
    O == O = True 
    Empty == Empty = True 
    _ == _ = False 

    x /= y = not (x == y)

showMark : Mark -> String 
showMark X = " X "
showMark O = " O "
showMark Empty = "   "

Board : Type 
Board = Matrix Mark 3 3 

export 
emptyBoard : Board 
emptyBoard = Vector.replicate (Vector.replicate Empty 3) 3

export 
showBoard : Board -> String 
showBoard board = Vector.showVec id "\n-----------\n" rows where 
    rows = Vector.map (\v => Vector.showVec showMark "|" v) board

public export
data Player : Type where 
    PlayerX : Player 
    PlayerO : Player 

export 
showPlayer : Player -> String 
showPlayer PlayerX = "PlayerX"
showPlayer PlayerO = "PlayerO"

getMark : Player -> Mark 
getMark PlayerX = X
getMark PlayerO = O

nextPlayer : Player -> Player 
nextPlayer PlayerX = PlayerO
nextPlayer PlayerO = PlayerX

public export 
record GameState where
    constructor MkGameState
    currentBoard : Board
    currentPlayer : Player 


public export 
record Position where 
    constructor MkPosition 
    x : Fin 3
    y : Fin 3

export
transition : GameState -> Position -> GameState 
transition (MkGameState board player) (MkPosition x y) = MkGameState (insert board x y (getMark player)) (nextPlayer player)

export 
isEmpty : Board -> Position -> Bool 
isEmpty board (MkPosition x y) = (Matrix.getAt board x y) == Empty

export 
isFull : Board -> Bool 
isFull board = Vector.all (== True) (Vector.map (\row => Vector.all (/= Empty) row) board)

export 
checkWinner : Board -> Player -> Bool
checkWinner board player =  anyRowWin (Matrix.rows board) || anyRowWin (Matrix.cols board) || anyRowWin (Matrix.diagonals board) where 
    areSameMark : Vec Mark n -> Bool 
    areSameMark v = Vector.all (== (getMark player)) v
    anyRowWin : Vec (Vec Mark n) m -> Bool 
    anyRowWin rows = Vector.any (== True) (Vector.map areSameMark rows)

export 
getWinner : Board -> Maybe Player 
getWinner board = case checkWinner board PlayerX of 
    True => Just PlayerX 
    False => case checkWinner board PlayerO of
        True => Just PlayerO 
        False => Nothing 
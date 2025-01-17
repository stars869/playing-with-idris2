module Tictactoe

import Vector
import Matrix 
import Finite 

public export 
data Mark : Type where 
    X : Mark
    O : Mark 
    Empty : Mark 

eq : Mark -> Mark -> Bool 
eq X X = True 
eq O O = True 
eq Empty Empty = True 
eq _ _ = False 

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
isEmpty board (MkPosition x y) = eq (Matrix.getAt board x y) Empty
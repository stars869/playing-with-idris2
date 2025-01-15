module Tictactoe

import Vector
import Matrix 

public export 
data Mark : Type where 
    X : Mark
    O : Mark 
    Empty : Mark 

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
showBoard board = (Vector.showVec id "\n-----------\n" rows) ++ "\n" where 
    rows = Vector.map (\v => Vector.showVec showMark "|" v) board


data Player : Type where 
    PlayerX : Player 
    PlayerO : Player 

record GameState where
    constructor MkGameState
    currentBoard : Board
    currentPlayer : Player 
    
module Main

import Tictactoe 


gameLoop : IO ()
gameLoop = putStr (Tictactoe.showBoard Tictactoe.emptyBoard)

main : IO ()
main = putStrLn "Tic tac toe?"
    >>= (\_ => gameLoop)

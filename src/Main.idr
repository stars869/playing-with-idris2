module Main

import Tictactoe 
import Finite 

import Data.String

toFin3 : Int -> Maybe (Fin 3)
toFin3 0 = Just FZ
toFin3 1 = Just (FS FZ) 
toFin3 2 = Just (FS (FS FZ))
toFin3 _ = Nothing 

parsePos : String -> Maybe Position
parsePos s = case String.words s of 
    [xStr, yStr] => (String.parseInteger xStr) >>= 
        (\x => (toFin3 x) >>= 
        (\fx => (String.parseInteger yStr) >>= 
        (\y => (toFin3 y) >>= 
        (\fy => Just (MkPosition fx fy))))) 
    _ => Nothing 

getPosInput : IO (Maybe Position)
getPosInput = do
    putStrLn "Enter position (e.g., \"0 1\"):"
    input <- getLine
    pure (parsePos input)

gameLoop : GameState -> IO ()
gameLoop state = do
    putStr (Tictactoe.showBoard (currentBoard state))
    mPos <- getPosInput 
    case mPos of 
        Just pos => gameLoop (Tictactoe.transition state pos)
        Nothing => gameLoop state

main : IO ()
main = do 
    putStrLn "Tic tac toe?"
    gameLoop (MkGameState Tictactoe.emptyBoard PlayerX)

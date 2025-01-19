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

getPosInput : GameState -> IO Position
getPosInput state = do
    input <- getLine 
    mPos <- pure (parsePos input) 
    case mPos of 
        Just pos => (if (Tictactoe.isEmpty (currentBoard state) pos) then pure pos else (putStrLn "Invalid position, please input again:" >>= (\_ => getPosInput state))) 
        Nothing => putStrLn "Invalid position, please input again:" >>= (\_ => getPosInput state)

gameLoop : GameState -> IO ()
gameLoop state = do 
    putStrLn (Tictactoe.showBoard (currentBoard state))
    putStrLn "Enter position (e.g., \"0 1\"):"
    pos <- getPosInput state 
    newState <- pure (Tictactoe.transition state pos)
    maybeWinner <- pure (Tictactoe.getWinner (currentBoard newState))
    case maybeWinner of 
        Just player => putStrLn ((Tictactoe.showPlayer player) ++ " has won!")
        Nothing => case Tictactoe.isFull (currentBoard newState) of 
            True => putStrLn "It's a tie~"
            False => gameLoop (newState) 
            
    
main : IO ()
main = do 
    putStrLn "Tic tac toe?"
    gameLoop (MkGameState Tictactoe.emptyBoard PlayerX)

import Dlx
import Queens
import Sudoku 
import Data.List.Split
import Data.List

-- Board exact cover problem 
data Board = Board {
    possibleInputs :: [String],
    numberOfInputs :: Int,
    boardState :: [[String]],
    conditions :: [String -> Bool]}

main = do 
    inputs <- getPossibleInputs
    let numberOfInputs = 4
    boardState <- getBoardState
    let conditions = getConditions
    return boardState

getPossibleInputs :: IO [String]
getPossibleInputs = do 
    putStrLn "Enter possible Inputs for your exact cover problem with a ' ' between them:"
    inputs <- getLine 
    inputs <- return $ splitOn " " inputs
    return inputs 

getBoardState :: IO [[String]]
getBoardState = do 
    putStrLn "Enter the initial board State. Use the inputs given before \n"
    board <- getBoard  
    return board where 
        getBoard = do
            putStrLn "Enter a board row. Use '.' if field is empty. Write 'n' if you are done:"
            row <- getLine
            if row == "n" then return [] else do
                row <- return $ splitOn "" row 
                (filter (\x -> x /= "") row :) <$> getBoard 

-- Conditions for the n queens problem 
-- How to create a dlx-Format from this? 
-- Maybe go through each condition one by one and generate the highest amount of items
-- Then combine them in some way that generates option
getConditions board = [all (==1) $ map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) board, 
                all (==1) $ map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (transpose board), 
                all (<=1) $ map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (getLRDiag board),
                all (<=1) $ map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (getLRDiag (map reverse board))]
    where 
        getLRDiag board = map (\i -> zipWith (!!) board [i..]) [0 .. length board -1] ++ helper board where 
            helper [] = []
            helper (x:xs) = zipWith (!!) xs [0..] : helper xs
            
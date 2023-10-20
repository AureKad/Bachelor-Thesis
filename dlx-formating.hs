import Dlx
import Queens
import Sudoku 
import Data.Char
import Data.List
{-
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

-}
-- Exported from Data.Universe.Helpers
diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
  go b es_ = [h | h:_ <- b] : case es_ of
    []   -> transpose ts
    e:es -> go (e:ts) es
    where ts = [t | _:t <- b]


rowCond :: [[String]] -> Bool
rowCond board = all (==1) (map (\xs -> sum (map (\x -> if x /= "." then 1 ::Int else 0 ::Int) xs )) board)

colCond :: [[String]] -> Bool 
colCond board = all (==1) (map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (transpose board))

lrDiag :: [[String]] -> Bool 
lrDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (diagonals board))

rlDiag :: [[String]] -> Bool 
rlDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (diagonals (reverse board)))

-- Testing if dlxFormat can output the right queens format based on only the conditions 
dlxFormatTest = let (items, options) = dlxFormat ([rowCond, colCond],[lrDiag, rlDiag]) [["-","-","-","-"],["-","-","-","-"],["-","-","-","-"],["-","-","-","-"]] in
    dlx items options 

test1 = rlDiag [[".","a"],["a","a"]]

-- get dlx format based on conditions and board 
dlxFormat (primaryCond, secondaryCond) board = let boardPos = boardPositions board 0 in 
    let options = nextCond (primaryCond ++ secondaryCond) boardPos boardPos 0 in 
    let (primary, secondary) = findItems options (length primaryCond) in 
        ((concat primary, concat secondary),options) where

        findItems opts i = splitAt i (map nub (transpose (map (drop 1) opts))) where   
        
        nextCond [] _ format _ = format 
        -- searches for next conditions format, combines it with the previous format, and saves it in format for the next function call
        nextCond (c:cs) allpos format i = nextCond cs allpos (combine format (nextPos c allpos [] [] 0 ([condLetter i]))) (i+1)

        condLetter i = chr (97+i)

        nextPos cond allpos provFormat store i lt = if length allpos == length provFormat then provFormat else 
            if test cond allpos (provFormat ++ [lt ++ show i]) 
                then nextPos cond allpos (provFormat ++ [lt ++ show i]) ((lt ++ show i) :store) (i+1) lt
                else nextPos cond allpos (findOpt cond allpos provFormat store) store i lt

        --Find critical option 
        findOpt _ _ x [] = error (show x)
        findOpt cond allpos provFormat (x:xs) = 
            let prov = map (\y -> if y == x then "." else y) provFormat in -- replaces x in provFormat to test if its the critical option
                if test cond allpos (prov ++ [x]) 
                then (provFormat ++ [x]) 
                else findOpt cond allpos provFormat xs

        test cond allpos provFormat = cond (translate (combine allpos provFormat)) 

        --provisional combine, aka for zipping allBoard with the work-in-progress formatting for condition
        combine _ [] = []
        combine (p:ps) (x:xs) = (p ++ [x]): combine ps xs  

        --dlf := dancing links formatting
        translate :: [[String]] -> [[String]]
        translate dlf = let transdlf = transform [] dlf in  
            let bdlf = bracketRows transdlf in 
                map (map (\x -> x!!1)) bdlf
            where
                bracketRows xss = 
                    let xs = bracketRowsHelper xss in 
                    let (res, _) = splitAt (length xss - length xs) xss in 
                        if xs == [] then [res] else res:bracketRows xs  

                bracketRowsHelper [x] = []
                bracketRowsHelper (x1:x2:xs) = if (x1!!0)!!1 == (x2!!0)!!1 then bracketRowsHelper (x2:xs) else x2:xs

                transform _ [] = []
                transform store (x:xs) = if elem (x!!1) store || (x!!1) == "." then [x!!0,"."]: transform store xs else 
                    [x!!0,"Q"]: transform ((x!!1):store) xs

        boardPositions [] _ = []
        boardPositions (xs:xss) i = boardCols xs i 0 ++ boardPositions xss (i+1)
            where
                boardCols [] _ _ = []
                boardCols (x:xs) i j = ["p" ++ show i ++ show j]: boardCols xs i (j+1)
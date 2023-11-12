{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Dlx
import DlxFormat ( getSymbolicFormat, getItems, interpret)
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad

data Input = Symbolic [String] | Shape [[String]]
data Board a = OneDim [String] | TwoDim [[String]] | Custom [a]
data Constraints = Constraints [Int] | ManualSelect

data Exact_Cover a = ECP {
    input :: Input,
    board :: Board a,
    primaryCons :: Constraints,
    secondaryCons :: Constraints
}
rectangle x y = [[""| x <- [1..x]] | y <- [1..y]]
--Queens 4
test = ECP {input = Symbolic ["Q"], board = TwoDim (rectangle 8 8), primaryCons = Constraints [1,2], secondaryCons = Constraints [3,4]}
--Queens 4 with an predetermined input 
test2 = ECP {input = Symbolic ["Q"], board = TwoDim [["","Q","",""],["","","",""],["","","",""],["","","",""]], primaryCons = Constraints [1,2], secondaryCons = Constraints [3,4]}
--Sudoku
test3 =  ECP {input = Symbolic [show i | i <- [1..9]], board = TwoDim hardSudokuPuzzle, primaryCons = Constraints [1,2,5], secondaryCons = Constraints []}

test4 = ECP {input = Symbolic ["Q"], board = TwoDim (rectangle 4 4), primaryCons = Constraints [1,6], secondaryCons = Constraints [3,4]}

test5 = ECP {input = Symbolic ["Q"], board = TwoDim (rectangle 2 2), primaryCons = Constraints [1,6], secondaryCons = Constraints []}

test6 = ECP {input = Symbolic [show i | i <- [1..7]], board = TwoDim filominoPuzzle, primaryCons = Constraints [7], secondaryCons = Constraints []}

sudokuPuzzle = [["","","","2","6","","7","","1"],["6","8","","","7","","","9",""],["1","9","","","","4","5","",""],
                ["8","2","","1","","","","4",""],["","","4","6","","2","9","",""],["","5","","","","3","","2","8"],
                ["","","9","3","","","","7","4"],["","4","","","5","","","3","6"],["7","","3","","1","8","","",""]]


hardSudokuPuzzle = [["","2","","","","","","",""],["","","","6","","","","","3"],["","7","4","","8","","","",""],
                    ["","","","","","3","","","2"],["","8","","","4","","","1",""],["6","","","5","","","","",""],
                    ["","","","","1","","7","8",""],["5","","","","","9","","",""],["","","","","","","","4",""]]

filominoPuzzle = [["","2","7","",""],["7","","","3","1"],["6","","","","7"],["","","6","",""],["6","","3","","1"]]

--solve :: Exact_Cover a -> IO ()
solve ecp =
    case input ecp of
        Symbolic inp ->
            case board ecp of
                OneDim brd -> return ()
                TwoDim brd ->
                    case primaryCons ecp of
                     Constraints prim ->
                            if not (all (\x -> x `elem` map fst condsSymbolic) prim) then error "Constraints not found" else
                                let condsSymbolicUpdate = filter (\(i, _) -> i `notElem` prim) condsSymbolic in
                                case secondaryCons ecp of
                                 Constraints sec ->
                                        if not (all (\x -> x `elem` map fst condsSymbolicUpdate) sec) then error "Constraints not found" else do
                                            options <- getSymbolicFormat (prim++sec) brd inp
                                            let items = getItems options (length prim)
                                            putStr (interpret brd (dlx items options))

        Shape inp -> return ()

condsSymbolic :: [(Int, String)]
condsSymbolic = [(1, "x inputs per row"), (2,"x inputs per column"), (3, "x inputs in the top left to bottom right diagonal"),
    (4, "x inputs in the top right to bottom left diagonal"), (5,"x inputs in an n*m box"), (6,"x inputs per custom shape"), 
    (7,"connected inputs"), (8,"no edges between inputs")]

main = do
    inputs <- getInputs
    board <- getBoard
    primary <- getConds condsSymbolic [] "primary"
    secondary <- getConds condsSymbolic primary "secondary"
    options <- getSymbolicFormat (primary ++ secondary) board inputs
    let items = getItems options (length primary)
    return (dlx items options)
    where
        getInputs = do
            putStrLn "What kind of Inputs do you have? (symbolic = 1, shape = 2)"
            kindInput <- getLine
            if kindInput == "1"
            then do
                putStrLn "Tell me the name of your inputs (with a ' ' between inputs)"
                splitOn " " <$> getLine
            else do return []

        getBoard = do
            putStrLn "Does your board have fixed inputs (y/n)"
            hasInputs <- getLine
            putStrLn "What kind of Board do you have? (1D = 1, 2D = 2, higher = 3)"
            kindBoard <- getLine
            if hasInputs == "y"
            then boardWithInputs kindBoard
            else boardWithoutInputs kindBoard

        boardWithInputs kindBoard =
            case kindBoard of
                "1" -> do
                    putStrLn "How many elements does your Board have (' ' between inputs, '-' if empty)"
                    row <- getLine
                    return [map (\x -> if x == "-" then "" else x) (splitOn " " row)]
                "2" -> do
                    putStrLn "How does your board look?"
                    putStrLn "Type the rows line by line \n'-' if elem empty, '.' if col doesn't exist in row, ' ' between each elem, and 'quit' if you are done"
                    typeBoard []

        boardWithoutInputs kindBoard =
            case kindBoard of
                "1" -> do
                    putStrLn "How many elements does your Board have"
                    n <- getLine
                    return [["" | x <- [1..(read n)]]]
                "2" -> do
                    board2d

        board2d = do
            putStrLn "What kind of shape does your board have? (rectangle = 1, custom = 2)"
            shape <- getLine
            if shape == "1" then do
                putStrLn "How many rows?"
                rows <- getLine
                putStrLn "How many columns?"
                cols <- getLine
                return [[""| x <- [1..(read cols)]] | y <- [1..(read rows)]]
            else do
                putStrLn "How does your board look?"
                putStrLn "Type the rows line by line \n'-' if elem empty, '.' if col doesn't exist in row, ' ' between each elem, and 'quit' if you are done"
                typeBoard []

        typeBoard board = do
            row <- getLine
            if row == "quit"
            then return board
            else do
                let filterdRow = map (\x -> if x == "-" then "" else x) (filter (/=".") (splitOn " " row))
                typeBoard (board ++ [filterdRow])

getConds conds unselectable str = do
    if str == "secondary" then do
        putStrLn "Does your problem have any secondary items? (y/n)"
        sec <- getLine
        if 'y' == toLower (head sec) then do
            putStrLn ("Select your "++ str ++ " Constraints\n")
            selectConds unselectable [] conds
        else return []
    else do
        putStrLn ("Select your "++ str ++ " Constraints\n")
        selectConds unselectable [] conds

selectConds :: [Int] -> [Int] -> [(Int, [Char])] -> IO [Int]
selectConds unselectable selected conds  = do
    let constraints = filter (\(i, _) -> i `notElem` (selected ++ unselectable)) conds
    putStrLn (foldr (\(i,n) r -> "("++ show i ++") " ++ n ++ "\n" ++ r) "" constraints ++ "Press 'q' to quit selection")
    cond <- getLine
    if 'q' == toLower (head cond) then helper (init selected)  (show (last selected)) else
        if read cond `notElem` map fst constraints
        then do
            putStrLn "I didn't understand that input. \n Write one of the shown Integers"
            selectConds unselectable selected conds
        else do
            putStrLn ("You selected: " ++ snd (head (filter (\(i,n) -> show i == cond) conds)))
            if length constraints == 1 then helper selected cond
            else do selectConds unselectable (selected ++ [read cond]) conds
        where
            helper selected  cond = do
                putStrLn "Are you sure about these Constraints: (y/n)"
                putStr (foldr (\x r -> snd ( head (filter (\(i,n) -> i == x) conds))++ "\n" ++ r) "" (selected ++ [read cond]))
                redo <- getLine
                if 'y' == toLower (head redo) then return (selected ++ [read cond]) else
                    selectConds unselectable [] conds







{- 
All Exact-Cover problems with 2 dimensions or lower that need only primary and secondary Constraints should be solvable with this dsl

data Exact_Cover_Problem = ECS {
    inputs :: [String], -- all the possible inputs for the problem
    board :: [[String]], -- the initial board state 
    primar Constraints :: [[[String]] -> Bool], -- the primary Constraints for the problem 
    secondar Constraints :: [[[String]] -> Bool], -- the secondary Constraints for the problem 
    -- some quality of life things  
    interpretResults :: Bool, -- fill in the dlx formatted solution into the initial board state 
    showInitialBoard :: Bool, -- show the initial board at the start of output 
    numberOfResults :: Int} -- how many results shown (negative int for all)


queens :: Int -> Exact_Cover_Problem
queens i = ECS {
    inputs = ["Q"],
    board = [["" | x <- [1..i]] | y <- [1..i]],
    primar Constraints = [rowCond,colCond],
    secondar Constraints = [lrDiag, rlDiag],
    interpretResults = True,
    showInitialBoard = False,
    numberOfResults = 2
    } where
    -- Exported from Data.Universe.Helpers
        diagonals :: [[a]] -> [[a]]
        diagonals = tail . go [] where
        go b es_ = [h | h:_ <- b] : case es_ of
            []   -> transpose ts
            e:es -> go (e:ts) es
            where ts = [t | _:t <- b]

        rowCond :: [[String]] -> Bool
        rowCond board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs )) board)

        colCond :: [[String]] -> Bool
        colCond board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs)) (transpose board))

        lrDiag :: [[String]] -> Bool
        lrDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs)) (diagonals board))

        rlDiag :: [[String]] -> Bool
        rlDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs)) (diagonals (reverse board)))

sudoku :: Exact_Cover_Problem
sudoku = ECS {
    inputs = [show i | i <- [1..9]],
    board = hardSudokuPuzzle,
    primar Constraints = [rowCondSudoku, colCondSudoku, boxCondSudoku],
    secondar Constraints = [],
    interpretResults = True,
    showInitialBoard = True,
    numberOfResults = 2
    } where

        rowCondSudoku :: [[String]] -> Bool
        rowCondSudoku board = let inputs = [show x | x <- [1..9]] in
            all (==True) (map (all (<=1)) (map (\input -> map (\xs -> sum (map (\x -> if x == input then 1 else 0) xs)) board ) inputs))

        colCondSudoku :: [[String]] -> Bool
        colCondSudoku board = let inputs = [show x | x <- [1..9]] in
            all (==True) (map (all (<=1)) (map (\input -> map (\xs -> sum (map (\x -> if x == input then 1 else 0) xs)) (transpose board) ) inputs))

        boxCondSudoku :: [[String]] -> Bool
        boxCondSudoku board = let inputs = [show x | x <- [1..9]] in
            all (==True) (map (all (<=1)) (map (\input -> map (\xs -> sum (map (\x -> if x == input then 1 else 0) xs)) (getBoxs board)) inputs)) where
                getBoxs board = concat (map (\i -> foldr (\j rs -> (foldr (\x r -> take 3 (drop (3*j) x) ++ r) []  (take 3 (drop (3*i) board))): rs ) [] [0..2]) [0..2])

-- https://sandiway.arizona.edu/sudoku/examples.html 

--sudoku takes a minute to compute =)
sudokuPuzzle = [["","","","2","6","","7","","1"],["6","8","","","7","","","9",""],["1","9","","","","4","5","",""],
                ["8","2","","1","","","","4",""],["","","4","6","","2","9","",""],["","5","","","","3","","2","8"],
                ["","","9","3","","","","7","4"],["","4","","","5","","","3","6"],["7","","3","","1","8","","",""]]


hardSudokuPuzzle = [["","2","","","","","","",""],["","","","6","","","","","3"],["","7","4","","8","","","",""],
                    ["","","","","","3","","","2"],["","8","","","4","","","1",""],["6","","","5","","","","",""],
                    ["","","","","1","","7","8",""],["5","","","","","9","","",""],["","","","","","","","4",""]]
-- mein dlxFormating hat probleme wenn erwartet wird dass ein input mehrfach für eine bedingung auftauchen soll   
ian :: Exact_Cover_Problem
ian = ECS {
    inputs = [show i | i <- [1..4]],
    board = [["" | x <- [1..6]] | x <- [1..6]],
    primar Constraints = [horizontalCond],
    secondar Constraints = [],
    interpretResults = True,
    showInitialBoard = True,
    numberOfResults = 1
    } where
rowCond :: [[String]] -> Bool
rowCond board = let inputs = [x | x <- [1..3]] in
    all ((==True) . all (==True)) (map (\input -> (map (\xs -> sum (map (\x -> if x == (show input) then 1 else 0) xs)<=input) board )) inputs)

colCond :: [[String]] -> Bool
colCond board = let inputs = [x | x <- [1..3]] in
    all (==True) (map (all (==True)) (map (\input -> (map (\xs -> sum (map (\x -> if x == (show input) then 1 else 0) xs) <= input) (transpose board)) ) inputs))

horizontalCond :: [[String]] -> Bool
horizontalCond = all connected where
        connected [x] = True
        connected (x:y:xs) = (x == "" || x /= y) && connected (y:xs)

verticalCond :: [[String]] -> Bool
verticalCond board = all connected (transpose board) where
        connected [x] = True
        connected (x:y:xs) = (x /= y) && connected (y:xs)
testCond = horizontalCond [["1","2","3","-","-","-","-","-","-"], ["-","-","-","-","-","-","-","-","-","-"]]
-}
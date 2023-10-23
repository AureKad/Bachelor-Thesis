import Dlx 
import DlxFormating
import Data.Char
import Data.List

{- 
All Exact-Cover problems with 2 dimensions or lower that need only primary and secondary conditions should be solvable with this dsl
-}
data Exact_Cover_Problem = ECS {
    inputs :: [String], -- ALl the possible inputs for the problem
    board :: [[String]], -- The initial board state 
    primaryConditions :: [[[String]] -> Bool], -- the primary conditions for the problem 
    secondaryConditions :: [[[String]] -> Bool], -- the secondary conditions for the problem 
    -- some quality of life things  
    interpretResults :: Bool, -- fill in the dlx formatted solution into the initial board state 
    showInitialBoard :: Bool, -- show the initial board at the start of output 
    numberOfResults :: Int} -- How many results shown (negative int for all)



solve :: Exact_Cover_Problem -> IO ()
solve ecs = let (items, options) = dlxFormat (primaryConditions ecs, secondaryConditions ecs) (board ecs) (inputs ecs) in
    let solutions = dlx items options in
    let result = "Number of solutions: " ++ show (length solutions) ++ "\n" in
    let initialBoard = "Initial Board: \n" ++ interpret (board ecs) [[]] in
    let trimmed = trimSols (numberOfResults ecs) solutions in 
        if showInitialBoard ecs && interpretResults ecs then putStr (initialBoard ++ interpret (board ecs) trimmed ++ result) 
        else
        if showInitialBoard ecs then putStr (initialBoard ++ displaySolutions trimmed ++ result) else
        if interpretResults ecs then putStr (interpret (board ecs) trimmed ++ result) 
        else putStr (displaySolutions trimmed ++ result) where 

            trimSols :: Int -> [[[String]]] -> [[[String]]]
            trimSols i sols = if length sols <= i || i < 0 then sols else
                take i sols  


queens :: Int -> Exact_Cover_Problem
queens i = ECS {
    inputs = ["Q"],
    board = [["" | x <- [1..i]] | y <- [1..i]],
    primaryConditions = [rowCond,colCond],
    secondaryConditions = [lrDiag, rlDiag],
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
    primaryConditions = [rowCondSudoku, colCondSudoku, boxCondSudoku],
    secondaryConditions = [],
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

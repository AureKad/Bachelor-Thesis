{-
This is Chapter 5 of my bachelor thesis (aka my contribution).
This module is about the dsl which is able to solve all shape based problems without needing to 
code anything, or to input the dlx format per hand.
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Dlx
import DlxFormat ( getSymbolicFormat, getItems, interpret, interpretConnectedInputs)
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad

data Input = InputsAndOccurences [(Int, String)] | ManuallySelectOccurence [String]
type Board = [[String]]
data Constraints = Constraints [Int]
data Exact_Cover = ECP {
    input :: Input,
    board :: Board,
    primaryCons :: Constraints,
    secondaryCons :: Constraints,
    showInitial :: Bool -- show initial board
}
-- See Chapter 5.1 for further explanation to these functions

-- With this function we can create a x*y empty board
rectangle :: (Num t1, Num t2, Enum t1, Enum t2) => t2 -> t1 -> [[String]]
rectangle x y = [[""| x <- [1..x]] | y <- [1..y]]

-- With this function we can give each input the same occurence rate in shape
occurEach :: [a] -> t -> [(t, a)]
occurEach inputs i = map (i,) inputs

-- With this function we can give one input an occurence rate in shape
occurs :: b -> a -> [(a, b)]
occurs input i = [(i, input)]

-- This is a transitional function 
next :: [a] -> [a] -> [a]
next a b = a ++ b

-- Empty list
empty :: [a]
empty = []

--Queens 4
test = ECP {input = InputsAndOccurences ("Q" `occurs` 1),
            board = rectangle 8 8,
            primaryCons = Constraints (rowCons `next` colCons),
            secondaryCons = Constraints (downDiagCons `next` upDiagCons),
            showInitial = True}
--Queens 4 with an predetermined input 
test2 = ECP {input = ManuallySelectOccurence ["Q"],
            board = [["","Q","",""],["","","",""],["","","",""],["","","",""]],
            primaryCons = Constraints (rowCons `next` colCons),
            secondaryCons = Constraints (downDiagCons `next` upDiagCons),
            showInitial = True}
--Sudoku
test3 = ECP {input = InputsAndOccurences ([show i | i <- [1..9]] `occurEach` 1),
            board = sudokuPuzzle,
            primaryCons = Constraints (rowCons `next` colCons `next` boxCons),
            secondaryCons = Constraints empty,
            showInitial = True}

test4 = ECP {input = InputsAndOccurences (["Q"] `occurEach` 1),
            board = rectangle 2 2,
            primaryCons = Constraints (rowCons `next` customCons),
            secondaryCons = Constraints empty,
            showInitial = True}

test5 = ECP {input = InputsAndOccurences (("1" `occurs` 1) `next` ("2" `occurs` 2) `next` ("3" `occurs` 3) `next` ("4" `occurs` 4)
                                          `next` ("5" `occurs` 5) `next` ("6" `occurs` 6) `next` ("7" `occurs` 7)),
            board = filominoPuzzle,
            primaryCons = Constraints connectedInputs,
            secondaryCons = Constraints empty,
            showInitial = True}
--row Constraint
rowCons :: [Int]
rowCons = [1]
--column Constraint
colCons :: [Int]
colCons = [2]
--downward diagonal Constraint
downDiagCons :: [Int]
downDiagCons = [3]
--upward diagonal Constraint
upDiagCons :: [Int]
upDiagCons = [4]
--box Constraint
boxCons :: [Int]
boxCons = [5]
--custom shape Constraint
customCons :: [Int]
customCons = [6]
--connected inputs (fillomino) Constraint
connectedInputs :: [Int]
connectedInputs = [7]

condsSymbolic :: [(Int, String)]
condsSymbolic = [(1, "x inputs per row"), (2,"x inputs per column"), (3, "x inputs in the top left to bottom right diagonal"),
    (4, "x inputs in the top right to bottom left diagonal"), (5,"x inputs in an n*m box"), (6,"x inputs per custom shape"),
    (7,"connected inputs")]

sudokuPuzzle = [["","","","2","6","","7","","1"],["6","8","","","7","","","9",""],["1","9","","","","4","5","",""],
                ["8","2","","1","","","","4",""],["","","4","6","","2","9","",""],["","5","","","","3","","2","8"],
                ["","","9","3","","","","7","4"],["","4","","","5","","","3","6"],["7","","3","","1","8","","",""]]


hardSudokuPuzzle = [["","2","","","","","","",""],["","","","6","","","","","3"],["","7","4","","8","","","",""],
                    ["","","","","","3","","","2"],["","8","","","4","","","1",""],["6","","","5","","","","",""],
                    ["","","","","1","","7","8",""],["5","","","","","9","","",""],["","","","","","","","4",""]]

filominoPuzzle = [["","2","7","",""],["7","","","3","1"],["6","","","","7"],["","","6","",""],["6","","3","","1"]]

{-
Description: Solves an exact cover problem. At this time only shape based and fillomino problems
Input: Exact_Cover data structure
Output: solutions printed out
-}
solve :: Exact_Cover -> IO ()
solve ecp = do
  let brd = board ecp
  let prim = removePrefix (primaryCons ecp)
  let sec = removePrefix (secondaryCons ecp)
  checkValidConstraints prim
  checkValidConstraints sec
  let inp = case input ecp of
        -- Every constraint will ask about the occurence rate
        ManuallySelectOccurence inputs -> occurEach inputs (-1) 
        InputsAndOccurences inputs -> inputs
  (items, options) <- getSymbolicFormat (prim++sec) brd inp (length prim)
  let initial = if showInitial ecp then initialBoard brd else ""
  let solutions = dlx items options
  -- Ugly extension of a non shape constraint
  if prim++sec == [7] then
    putStr (initial ++ interpretConnectedInputs brd solutions)
  else putStr (initial ++ interpret brd solutions ++ "There are " ++ show (length solutions) ++ " solutions to this puzzle\n")
    where
      removePrefix (Constraints cons) = cons

      checkValidConstraints cons =
        unless (all (\x -> x `elem` map fst condsSymbolic) cons) $ error "Constraints not found"

{-
Description: Returns a string of the initial board state 
Input: Board (from Exact_Cover)
Output: String of initial board
-}
initialBoard :: [[String]] -> String
initialBoard board = let init = "Initial board:\n" in
  let results = "Results:\n" in
  init ++ foldr (\xs r -> foldr (\x r2 -> if x == "" then "- " ++ r2 else x ++ " " ++ r2) "" xs++ "\n" ++ r) "\n" board ++ results
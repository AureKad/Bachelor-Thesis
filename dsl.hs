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
data Constraints = Constraints [Int] | ManualSelect
data Exact_Cover = ECP {
    input :: Input,
    board :: Board,
    primaryCons :: Constraints,
    secondaryCons :: Constraints,
    showInitial :: Bool -- show initial board
}
rectangle :: (Num t1, Num t2, Enum t1, Enum t2) => t2 -> t1 -> [[String]]
rectangle x y = [[""| x <- [1..x]] | y <- [1..y]]

occurEach :: [a] -> t -> [(t, a)]
occurEach inputs i = map (i,) inputs

occurs :: b -> a -> [(a, b)]
occurs input i = [(i, input)]              

next :: [a] -> [a] -> [a] 
next a b = a ++ b

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

rowCons :: [Int]
rowCons = [1]
colCons :: [Int]
colCons = [2]
downDiagCons :: [Int]
downDiagCons = [3]
upDiagCons :: [Int]
upDiagCons = [4]
boxCons :: [Int]
boxCons = [5]
customCons :: [Int]
customCons = [6]
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

solve :: Exact_Cover -> IO ()
solve ecp =
  let brd = board ecp in
  case primaryCons ecp of
    ManualSelect -> return () 
    Constraints prim ->
      if not (all (\x -> x `elem` map fst condsSymbolic) prim) then error "Constraints not found" else
        let condsSymbolicUpdate = filter (\(i, _) -> i `notElem` prim) condsSymbolic in
        case secondaryCons ecp of
          ManualSelect -> return () 
          Constraints sec ->
            if not (all (\x -> x `elem` map fst condsSymbolicUpdate) sec) then error "Constraints not found" else do
              let inp = case input ecp of 
                    ManuallySelectOccurence inputs -> occurEach inputs (-1)
                    InputsAndOccurences inputs -> inputs
              (items, options) <- getSymbolicFormat (prim++sec) brd inp (length prim)
              let initial = if showInitial ecp then initialBoard brd else ""
              let solutions = dlx items options
              if prim++sec == [7] then
                putStr (initial ++ interpretConnectedInputs brd solutions)
              else putStr (initial ++ interpret brd solutions ++ "There are " ++ show (length solutions) ++ " solutions to this puzzle\n")

initialBoard :: [[String]] -> String
initialBoard board = let init = "Initial board:\n" in
  let results = "Results:\n" in
  init ++ foldr (\xs r -> foldr (\x r2 -> if x == "" then "- " ++ r2 else x ++ " " ++ r2) "" xs++ "\n" ++ r) "\n" board ++ results

{-
main = do
  inputs <- getInputs
  board <- getBoard
  primary <- getConds condsSymbolic [] "primary"
  secondary <- getConds condsSymbolic primary "secondary"
  (items, options) <- getSymbolicFormat (primary ++ secondary) board inputs (length primary)
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
-}
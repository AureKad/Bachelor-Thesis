module Dlx (dlx, displaySolutions) where
import DllArray 
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Data.List
import Debug.Trace
import Queens 
import Sudoku


dlx :: Eq a => ([a],[a]) -> [[a]] -> [[[a]]]
dlx items options = runST (dlxInit items options) where
  --X1 
  dlxInit :: Eq a => ([a],[a]) -> [[a]] -> ST s [[[a]]]
  dlxInit (primary,secondary) options = do
    items <- itemArray (primary++secondary)
    nodes <- nodeArray options items
    bt <- newArray (0, length options) 0 --backtrack Array
    let n = length primary  --delimiter between primary and secondary items
    let l = 0 
    
    r <- getNext items l
    solutionIndices <- dlxLoop items nodes bt l n
    interpretSolution items nodes (map sort solutionIndices) where

  --X2
  dlxLoop ::  STArray s Int (Item a) -> STArray s Int Node -> STArray s Int Int -> Int -> Int -> ST s [[Int]]
  dlxLoop items nodes bt l n = do 
    fin <- finished items n
    if fin 
      then do solution <- getElems bt
              (take l solution:) <$> leaveLevelL items nodes bt l n
      else do i <- chooseNext items nodes n --X3
              --X4  
              cover items nodes i
              x <- getDown nodes i 
              modifyArray bt l (\_ -> x) 
              --X5
              try items nodes bt l i n
              

  finished :: STArray s Int (Item a) -> Int -> ST s Bool 
  finished items n = do 
    i <- getNext items 0 
    return ( i==0 || i>n )

  --X3
  --choose next item to be covered with the MRV heuristic (page 121)
  chooseNext :: STArray s Int (Item a) -> STArray s Int Node -> Int -> ST s Int 
  chooseNext items nodes n = do
    p <- getNext items 0 
    o <- getItm nodes p 
    helper items nodes o p p n where 
      helper items nodes o p i n = do
        if p == 0 || p >= n then return i else do 
          lambda <- getItm nodes p
          p' <- getNext items p
          if lambda >= o && p /= 0 then helper items nodes o p' i n else do 
            helper items nodes lambda p' p n

  --X5
  try ::  STArray s Int (Item a) -> STArray s Int Node -> STArray s Int Int -> Int -> Int -> Int -> ST s [[Int]]
  try items nodes bt l i n = do 
    xl <- readArray bt l
    if xl == i 
      then do uncover items nodes i --X7
              leaveLevelL items nodes bt l n
      else do let p = xl + 1 
              whileLoop (\p -> p /= xl) p items nodes 
              dlxLoop items nodes bt (l+1) n where 

      whileLoop cond p items nodes = do 
        if not (cond p) then return () else do 
          j <- getItm nodes p 
          if j == -1 
            then do p' <- getUp nodes p 
                    whileLoop cond p' items nodes 
            else do cover items nodes j 
                    whileLoop cond (p+1) items nodes 
  --X6
  retry :: STArray s Int (Item a) -> STArray s Int Node -> STArray s Int Int -> Int -> Int -> ST s [[Int]]
  retry items nodes bt l n = do 
    xl <- readArray bt l 
    let p = xl - 1 
    whileLoop (\p -> p /= xl) p items nodes 
    i <- getItm nodes xl 
    xl' <- getDown nodes xl 
    modifyArray bt l (\_ -> xl') 
    try items nodes bt l i n where 

      whileLoop cond p items nodes = do 
        if not (cond p) then return () else do 
          j <- getItm nodes p 
          if j == -1 
            then do p' <- getDown nodes p 
                    whileLoop cond p' items nodes
            else do uncover items nodes j 
                    whileLoop cond (p-1) items nodes 
  --X8 
  leaveLevelL :: STArray s Int (Item a) -> STArray s Int Node -> STArray s Int Int -> Int -> Int -> ST s [[Int]]
  leaveLevelL items nodes bt l n = 
    if l == 0 then return [] else do 
      retry items nodes bt (l-1) n

cover :: STArray s Int (Item a) -> STArray s Int Node -> Int -> ST s ()
cover items nodes i = do 
  p <- getDown nodes i 
  whileLoop (\p -> p /= i) hide p nodes
  l <- getPrev items i 
  r <- getNext items i 
  modifyArray items l (\n -> n {next = r})
  modifyArray items r (\n -> n {prev = l}) where 

    whileLoop :: (Int -> Bool) -> (Int -> STArray s Int Node -> ST s ()) -> Int -> STArray s Int Node -> ST s ()
    whileLoop cond hide p nodes = if not (cond p) then return () else do 
      hide p nodes 
      p' <- getDown nodes p
      whileLoop cond hide p' nodes

    hide :: Int -> STArray s Int Node -> ST s () 
    hide p nodes = do 
      let q = p + 1
      whileLoop (\q -> q /= p) q nodes where 
          
        whileLoop :: (Int -> Bool) -> Int -> STArray s Int Node -> ST s ()
        whileLoop cond q nodes = if not (cond q) then return () else do 
          x <- getItm nodes q 
          u <- getUp nodes q
          d <- getDown nodes q 
          if x <= 0 then whileLoop cond u nodes else do
            modifyArray nodes u (\n -> n {down = d})
            modifyArray nodes d (\n -> n {up = u})
            subTopLen nodes x
            whileLoop cond (q+1) nodes 

uncover :: STArray s Int (Item a) -> STArray s Int Node -> Int -> ST s ()
uncover items nodes i = do  
  l <- getPrev items i 
  r <- getNext items i 
  modifyArray items l (\n -> n {next = i})
  modifyArray items r (\n -> n {prev = i})
  p <- getUp nodes i 
  whileLoop (\p -> p /= i) unhide p nodes  where 

    whileLoop :: (Int -> Bool) -> (Int -> STArray s Int Node -> ST s ()) -> Int -> STArray s Int Node -> ST s ()
    whileLoop cond hide p nodes = if not (cond p) then return () else do 
      unhide p nodes 
      p' <- getUp nodes p
      whileLoop cond hide p' nodes

    unhide :: Int -> STArray s Int Node -> ST s () 
    unhide p nodes = do 
      let q = p - 1
      whileLoop (\q -> q /= p) q nodes where 
          
        whileLoop :: (Int -> Bool) -> Int -> STArray s Int Node -> ST s ()
        whileLoop cond q nodes = if not (cond q) then return () else do 
          x <- getItm nodes q 
          u <- getUp nodes q
          d <- getDown nodes q 
          if x <= 0 then whileLoop cond d nodes else do
            modifyArray nodes u (\n -> n {down = q})
            modifyArray nodes d (\n -> n {up = q})
            addTopLen nodes x
            whileLoop cond (q-1) nodes 

                    
interpretSolution :: STArray s Int (Item a) -> STArray s Int Node -> [[Int]] -> ST s [[[a]]]
interpretSolution items nodes [] = return []
interpretSolution items nodes (is:iss) = do
  sol <- printSolution items nodes is
  (sol:) <$> interpretSolution items nodes iss where 

  printSolution items nodes [] = return []
  printSolution items nodes (i:is) = do 
    p <- findStartOfOption nodes i
    option <- whileLoop p items nodes
    (option:) <$> printSolution items nodes is where

      findStartOfOption nodes p = do 
        j <- getItm nodes p 
        if j == -1 then return (p+1) else 
          findStartOfOption nodes (p-1)

      whileLoop p items nodes = do 
        j <- getItm nodes p 
        if j == -1 then return []
          else do elem <- getName items j  
                  (elem:) <$> whileLoop (p+1) items nodes

displaySolutions :: [[[String]]] -> String 
displaySolutions = foldr (\xss n -> displaySolution xss ++ n) "" where 
  displaySolution = foldr (\xs n -> concat (map (\x -> x ++ " ") xs) ++ "\n" ++ n) "\n" 

test = runSTArray (do
  items <- itemArray ["a","b","c","d","e","f","g"]
  nodes <- nodeArray [["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]] items
  return nodes) 

test1 = runSTArray (do
  items <- itemArray ["a","b","c","d","e","f","g"]
  nodes <- nodeArray [["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]] items
  cover items nodes 2
  return nodes)

test2 = runSTArray (do
  items <- itemArray ["a","b","c","d","e","f","g"]
  nodes <- nodeArray [["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]] items
  cover items nodes 2
  uncover items nodes 2
  return nodes)

test3 = test == test2


testdlx = dlx (["a","b","c","d","e","f","g"],[]) [["c","e"],["a","d","g"],["b","c","f"],["a","d","f"],["b","g"],["d","e","g"]]

testdlx1 = dlx (["a","b"],["c"]) [["a","b","c"], ["a","b"]]

testdlx2 = dlx (["a","b"],["c"]) [["a"],["c"]]

testdlx3 = dlx (["a","b"],["c"]) []

testdlx4 = dlx (["1", "2", "3","4", "5", "6", "7"],[]) [["1","4"], ["1", "4", "7"], ["2","7"], ["3","5","6"], ["4","5","7"],["2","3","6","7"]]

testBA1 = dlx (["A","B","C","D","E","F"],[]) [["A","B"],["A","B","C"],["C","E"],["D","F"],["E","F"]] 

testBA2 = dlx (["A","B"],["C"]) [["A","B","C"], ["A","B"], ["C"]]

testQueens i = let (items, options) = queens i in 
  putStr (interpretQueens i (dlx items options))
    
-- https://sandiway.arizona.edu/sudoku/examples.html
testSudoku = let board = ["...26.7.1","68..7..9.","19...45","82.1...4","..46.29",".5...3.28","..93...74",".4..5..36","7.3.18"] in
    let (items, options) = sudoku board in 
       putStr (interpretSudoku board (dlx items options))

testSudoku2 = let board = [".2","...6....3",".74.8",".....3..2",",8..4..1","6..5","....1.78","5....9",".......4"] in -- real hard 
  let (items, options) = sudoku board in 
    putStr (interpretSudoku board (dlx items options))

testSudoku3 = let board = ["9.6.7.4.3","...4..2",".7..23.1","5.....1",".4.2.8.6","..3.....5",".3.7...5","..7..5","4.5.1.7.8"] in --multiple solutions
  let (items, options) = sudoku board in 
    putStr (interpretSudoku board (dlx items options))

testSudokuBA = let board = [".2.5.1.9.","8..2.3..6",".3..6..7.","..1...6..","54.....19","..2...7..",".9..3..8.","2..8.4..7",".1.9.7.6."] in 
  let (items, options) = sudoku board in 
    putStr (interpretSudoku board (dlx items options))
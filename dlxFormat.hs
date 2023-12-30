{-
Chapter 5 of my bachelor thesis. This is the main contribution. 
Creates a dlx format from given inputs, board, and constraints that can be used to find 
solutions with dlx. 
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use guards" #-}

module DlxFormat where
import Data.Char
import Data.List.Split
import Data.List
import ConnectedInputs
{-
Description: We get the format from this function (Algorithm 12)
Input: Constraints, initial board, all possible inputs, how many primary constraints there are
Output: dlxFormat ((primary items, secondary items), options)
-}
getSymbolicFormat :: [Int] -> [[String]] -> [(Int,String)] -> Int -> IO (([String],[String]),[[String]])
getSymbolicFormat cons board inputs prim = do
  putStrLn "Let's go deeper into the selected constraints\n"
  let conLetterNumber = 97
  if cons == [7] then do
    options <- getSymbolicOptions 7 board inputs conLetterNumber
    return (getItems (map ("X":) (concat options)) 1, (concat options))
  else do
    options <- getSymbolicFormatHelper cons board inputs (map (:[]) (concat (boardPositions id board))) conLetterNumber
    let opts = filter (\x-> length x > 2 && notElem "X" x) (concat options)
    return (getItems opts prim, opts)
  where

    getSymbolicFormatHelper [] _ _ format _  = return format
    getSymbolicFormatHelper (c:cons) board inputs format ltNumber = do

      options <- getSymbolicOptions c board inputs ltNumber
      getSymbolicFormatHelper cons board inputs (combine format (removePos options)) (ltNumber+1)

    removePos :: [[[String]]] -> [[String]]
    removePos = map (foldr (\x r -> tail x ++ r) [])

{-
Description: Continuation of Algorithm 12, inner part of for loop
  Transform function are used here (see Chapter 5.2.2)
Input: Constraint, initial board, all possible inputs, letter used for constraint
Output: Options (with extra list around each option group (the ones that have the same cell))
-}
getSymbolicOptions :: Int -> [[String]] -> [(Int, String)] -> Int -> IO [[[String]]]
getSymbolicOptions con board inputs conNumber =
  case con of
    1 -> rowCon where
      rowCon =
        if fst (head inputs) == -1 then
          manuallySelectInputAppearence board (map snd inputs) id "row" conNumber
        else return (getOptionRow board inputs (boardPositions id board) [chr conNumber])
    2 -> colCon where
      colCon = do
        options <- if fst (head inputs) == -1 then
                     manuallySelectInputAppearence board (map snd inputs) transpose "column" conNumber
                   else return (getOptionRow (transpose board) inputs (boardPositions transpose board) [chr conNumber])
        return (sort options (boardPositions id board))

    3 -> lrDiagCon where
      lrDiagCon = do
        options <- if fst (head inputs) == -1 then
                      manuallySelectInputAppearence board (map snd inputs) (diagonals . reverse) "top left to bottom right diagonal" conNumber
                   else return (getOptionRow ((diagonals . reverse) board) inputs (boardPositions (diagonals . reverse) board) [chr conNumber])
        return (sort options (boardPositions id board))

    4 -> rlDiagCon where
      rlDiagCon = do
        options <- if fst (head inputs) == -1 then
                    manuallySelectInputAppearence board (map snd inputs) diagonals "bottom left to top right diagonal" conNumber
                   else return (getOptionRow (diagonals board) inputs (boardPositions diagonals board) [chr conNumber])
        return (sort options (boardPositions id board))

    5 -> boxCon where
      boxCon = do
        putStrLn "Box conition:"
        let loopWidth = do
              putStr "Width: "
              x <- getLine
              if not (all isDigit x)
              then do putStrLn "Not a valid Number"
                      loopWidth
              else return x
        let loopHeight = do
              putStr "Height: "
              y <- getLine
              if not (all isDigit y)
              then do putStrLn "Not a valid Number"
                      loopWidth
              else return y
        x' <- loopWidth
        let x = read x'
        y' <- loopHeight
        let y = read y'
        options <- if fst (head inputs) == -1 then
                     manuallySelectInputAppearence board (map snd inputs) (\brd -> getBoxs brd x y) "box" conNumber
                   else return (getOptionRow ((\brd -> getBoxs brd x y) board) inputs (boardPositions (\brd -> getBoxs brd x y) board) [chr conNumber])
        return (sort options (boardPositions id board))

      getBoxs board w h = concatMap (\i -> map
        (\ j -> foldr (\ x r -> take w (drop (w * j) x) ++ r) []
        (take h (drop (h * i) board))) [0..(div (length (head board)) w - 1)]) [0..(div (length board) h - 1)]

    6 -> customCon where
      customCon = do
        let maxCols = maximum (map length board)
        let rows = length board
        putStrLn "\nGive me the coordinates for the custom shapes:"
        let loopX = do
              putStr "X: "
              x <- getLine
              if x `notElem` ([show i | i <- [1..maxCols]]) && x /= "done"
              then do putStrLn "I didn't recognize this coordinate :("
                      loopX
              else return x
        let loopY = do
              putStr "Y: "
              y <- getLine
              if y `notElem` ([show i | i <- [1..rows]]) && y /= "done"
              then do putStrLn "I didn't recognize this coordinate :("
                      loopY
              else return y
        let loopShape board indices = do
              putStr (foldr (\(i,xs) r -> show i ++ "  " ++ foldr (\x r2 -> if x == "" then "- " ++ r2 else x ++ " " ++ r2) "" xs++ "\n" ++ r) ""
                (zip [0..] (([show i | i <- [1..maxCols]] ++ [" <- y"]): board)))
              putStrLn "Write 'done' into X if you are done with the shape"
              x <- loopX
              if x == "done" then do
                putStrLn "Are you happy with this shape? (y/n)"
                happy <- getLine
                if 'y' == toLower (head happy) then
                  return (board, indices) else return ([],[])
              else do
                y <- loopY
                if length (board!!(read y - 1)) < read x || board!!(read y-1)!!(read x-1) == "*" then do
                  putStrLn "This is an illegal field, please try again"
                  loopShape board indices
                else loopShape (replaceElem (read y-1) (read x-1) board "°") ((read y-1,read x-1):indices)
        let loop board = do
              (brd,shapeIndices) <- loopShape board []
              if null brd then do putStrLn "Let's redo it then!"
                                  loop board
              else do
                  putStrLn "Was this the last shape? (y/n)"
                  lastShape <- getLine
                  if 'y' == toLower (head lastShape) then
                      return [nub shapeIndices]
                  else do
                      let newBoard = map (map (\x -> if x == "°" then "*" else x)) brd
                      (nub shapeIndices:) <$> loop newBoard

        shapes <- loop board
        let shapeRows board = map (map (\(i,j) -> board!!i!!j)) shapes
        options <- if fst (head inputs) == -1 then
                     manuallySelectInputAppearence board (map snd inputs) shapeRows "custom shape" conNumber
                   else return (getOptionRow (shapeRows board) inputs (boardPositions shapeRows board) [chr conNumber])
        return (sort options (boardPositions id board))

    7 -> connectedInputsCon where
      connectedInputsCon = do
        inps <- if fst (head inputs) == -1 then do
            putStrLn "Tell me how often an Input is supposed to be connected with the same Input"
            getInputAppearence (map snd inputs) (\str -> "How often is " ++ str ++ " connected")
          else return inputs
        let options = connectedInputs board inps conNumber
        return [options]


    where
  sort :: [[[String]]] -> [[[String]]] -> [[[String]]]
  sort options pos = map (\p -> concat (filter (\ops -> head (head ops) == p) options)) (concat (concat pos))

  diagonals :: [[a]] -> [[a]]
  diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
      []   -> transpose ts
      e:es -> go (e:ts) es
      where ts = [t | _:t <- b]


{-
Description: Replaces an element in a list 
Input: Index of elem to be replaced, list, new elem
Output: new list 
-}
replaceElem1d :: Int -> [a] -> a -> [a]
replaceElem1d i list elem = let (lxs,_:rxs) = splitAt i list in
  lxs ++ [elem] ++ rxs

{-
Description: Replaces an element in a matrix
Input: Index list, Index elem, matrix, new elem
Output: new matrix
-}
replaceElem :: Int -> Int -> [[a]] -> a -> [[a]]
replaceElem a b board elem = let (lrows,xs:rrows) = splitAt a board in
  let (lxs,_:ys) = splitAt b xs in
    lrows ++ (lxs ++ [elem] ++ ys):rrows


{-
Description: Creates a p_ij elem for each elem in board and repositions the p_ij`s
  depending on transform function (Algorithm 9)
Input: transform function, initial board
Output: base case format (each p_ij is in a seperate list, see thesis for extra explanation)
-}
boardPositions :: ([[String]] -> [[String]]) -> [[String]] -> [[[String]]]
boardPositions transform board = map (map (:[])) (transform (
  zipWith (\ i xs -> boardCols xs i 0) [0..] board))
  where
    boardCols [] _ _ = []
    boardCols (x:xs) i j = ("p" ++ show i ++ show j): boardCols xs i (j+1)
{-
Description: Combines format with new items if they have the same input (Algorihtm 11)
Input: format, new options/items
Output: combined format
-}
combine :: [[[String]]] -> [[String]] -> [[[String]]]
combine [] _ = []
combine (fs:fss) (n:ns) =
  foldr (\f r -> [f ++ [x] | x <- n, length f == 1 || last (splitOn "-" x) == last (splitOn "-" (f!!1)) ] ++ r) [] fs : combine fss ns

{-
Description: Manually select the occurence of inputs in constraint (Algorithm 8)
Input: initial board (not needed), inputs, transform function (not needed), String, constraint letter
Output: format (since it call getConfirmation which calls getSymbolicOptions)
-}
manuallySelectInputAppearence :: [[String]] -> [String] -> ([[String]] -> [[String]]) -> [Char] -> Int -> IO [[[String]]]
manuallySelectInputAppearence board inputs transform shape conNumber  = do
  putStrLn (toUpper (head shape): tail shape ++ " conition:")
  putStrLn ("One Input per " ++ shape++"? (y/n)")
  oneInput <- getLine
  if 'y' == toLower (head oneInput)
  then getConfirmation 1 board (map (1,) inputs) (getOptionRow (transform board) (map (1,) inputs) (boardPositions transform board) [chr conNumber]) conNumber
  else do
    putStrLn ("Tell me how often an Input appears (at most) per " ++ shape)
    inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the row?")
    getConfirmation 1 board inputAppearence (getOptionRow (transform board) inputAppearence (boardPositions transform board) [chr conNumber]) conNumber

{-
Description: Continuation of Algorithm 8, write the input appearence into console
Input: inputs, string to output
Output: occurence rate of each input
-}
getInputAppearence :: [String] -> (String -> String) -> IO [(Int, String)]
getInputAppearence [] str = return []
getInputAppearence (input:inputs) str = do
  putStrLn (str input)
  i <- getLine
  if all isDigit i then
    ((read i,input):) <$> getInputAppearence inputs str
  else do
    putStrLn "Not a valid number :("
    getInputAppearence inputs str
{-
Description: Asks for confirmation for input occurence
Input: Occurence rate, initial board, inputs, format, constraint number/letter
Output: format (since it call getSymbolicOptions)
-}
getConfirmation :: Int -> [[String]] -> [(Int, String)] -> [[[String]]] -> Int -> IO [[[String]]]
getConfirmation i board inputs formatFunc conNumber = do
  putStrLn (foldr (\(i,input) r -> "Input " ++ input ++ " appears " ++ show i ++ " times\n" ++ r) "" inputs)
  putStrLn "Are you sure about this? (y/n)"
  oneInput <- getLine
  if 'y' == toLower (head oneInput)
  then return formatFunc
  else getSymbolicOptions i board (map (-1,) (map snd inputs)) conNumber

{-
Description: All shape based constraints are reduced with the transform function so that
  this function can give out the correct format/options (Algorithm 10)
Input: initial board, inputs, base format (only positional items), letter for constraint
Output: format/options for constraint
-}
getOptionRow :: [[String]] -> [(Int, String)] -> [[[String]]] -> String -> [[[String]]]
getOptionRow board inputs pos lt =
  let options = map (oneInputFormat board) inputs in
  combine (map (:[]) (concat pos)) (concatItems options [])
    where
    oneInputFormat board (i,input) = let inputAmount = map (\xs -> i - length (filter (==input) xs)) board in
      let itemDigits = scanl (+) 0 inputAmount in
        concatMap (\(j,xs) -> map (\x -> if not (null x) then [] else
          -- If no input of this con can be put unto x, then no input in general can go into this field (this signifies the ["X"])
          -- All the fields get deleted because of combines 'splitOn "-"'
          if null [(itemDigits!!j)..(itemDigits!!(j+1)-1)] then ["X"] else
              map (\k -> lt ++ show k ++ "-" ++ input) [(itemDigits!!j)..(itemDigits!!(j+1)-1)]) xs) (zip [0..] board)

    concatItems (o:opts) [] = concatItems opts o
    concatItems [] result = result
    concatItems (o:opts) result = concatItems opts (zipWith (++) result o)

{-
Description: Gets the primary and secondary items of the options based on the 
  amount of primary constraints
Input: Options, length of primary constraints
Output: (primary constraints, secondary constraints) (positional items are secondary items)
-}
getItems :: [[String]] -> Int ->  ([String], [String])
getItems opts i  = let conLetters = sort (tail (nub (map (take 1) (concat opts)))) in
  let items = map (\lt -> nub (filter (\x -> lt == take 1 x) (concat opts))) (conLetters ++ ["p"]) in
  let (pr, sr) = splitAt i items in
  (concat pr, concat sr)

{-
Description: Interprets the solutions of dlx that were found using this format (Chapter 5.2.3 Algorithm 13)
Input: initial board, solutions from dlx
Output: solution string
-}
interpret :: [[String]] -> [[[String]]] -> String
interpret board solutions = let result = map (\x -> insertRow (transform x) board 0) solutions in
  foldr (\x r -> print x ++ r) "" result where
  print = foldr (\xs r -> foldr (\x r2 -> if x == "" then "- " ++ r2 else x ++ " " ++ r2) "" xs++ "\n" ++ r) "\n"
  insertRow _ [] _ = []
  insertRow ps (xs:xss) i = let (psUpdate, row) = insertCol ps xs i 0 [] in
    row: insertRow psUpdate xss (i+1)

  insertCol ps [] _ _ row = (ps, row)
  insertCol [] (x:xs) i j row = insertCol [] xs i (j+1) (row ++ [x])
  insertCol ((p,inp):ps) (x:xs) i j row =
    if "p" ++ show i ++ show j == p then insertCol ps xs i (j+1) (row ++ [inp]) else
      insertCol ((p,inp):ps) xs i (j+1) (row ++ [x])

  transform =  map (\x -> (head x, getInput (x!!1)))

  getInput x = let input = splitOn "-" x in last input

{-
Description: Interprets Fillomino (needs different function since each option is not only 
  for one cell)
Input: initial board, solutions from dlx
Output: solution string
-}
interpretConnectedInputs :: [[String]] -> [[[String]]] -> String
interpretConnectedInputs board solutions = let prep = map (\xss -> transform xss) solutions in
  let results = map (insertInputs board) prep in
    foldr (\x r -> print x ++ r) "" results where

  print = foldr (\xs r1 -> foldr (\x r2 -> x ++ " " ++ r2) "" xs++ "\n" ++ r1) "\n"

  insertInputs _ [] = []
  insertInputs board xs = foldr (flip insertInput) board xs

  insertInput board x = let coords = splitOn "-" (tail (fst x)) in
    let i = read (head coords) in
    let j = read (last coords) in
      replaceElem i j board (snd x)

  getInput xs = show (length (filter (\x -> head x == 'a') xs))

  transform = concatMap (\xs -> map (\y -> (y, getInput xs)) (filter (\x -> head x == 'a') xs))


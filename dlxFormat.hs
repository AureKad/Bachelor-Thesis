{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use guards" #-}

module DlxFormat where
import Data.Char
import Data.List.Split
import Data.List
import ConnectedInputs

getSymbolicFormat :: [Int] -> [[String]] -> [(Int,String)] -> Int -> IO (([String],[String]),[[String]])
getSymbolicFormat conds board inputs prim = do
  putStrLn "Let's go deeper into the selected constraints\n"
  let condLetterNumber = 97
  if conds == [7] then do
    options <- getSymbolicOptions 7 board inputs condLetterNumber
    return (getItems (map ("X":) (concat options)) 1, (concat options))
  else do
    options <- getSymbolicFormatHelper conds board inputs (map (:[]) (concat (boardPositions id board))) condLetterNumber
    let opts = filter (\x-> length x > 2 && notElem "X" x) (concat options)
    return (getItems opts prim, opts)
  where

    getSymbolicFormatHelper [] _ _ format _  = return format
    getSymbolicFormatHelper (c:conds) board inputs format ltNumber = do

      options <- getSymbolicOptions c board inputs ltNumber
      getSymbolicFormatHelper conds board inputs (combine format (removePos options)) (ltNumber+1)

    removePos :: [[[String]]] -> [[String]]
    removePos = map (foldr (\x r -> tail x ++ r) [])

getSymbolicOptions :: Int -> [[String]] -> [(Int, String)] -> Int -> IO [[[String]]]
getSymbolicOptions cond board inputs condNumber =
  case cond of
    1 -> rowCond where
      rowCond =
        if fst (head inputs) == -1 then
          manuallySelectInputAppearence board (map snd inputs) id "row" condNumber
        else return (getOptionRow board inputs (boardPositions id board) [chr condNumber])
    2 -> colCond where
      colCond = do
        options <- if fst (head inputs) == -1 then
                     manuallySelectInputAppearence board (map snd inputs) transpose "column" condNumber
                   else return (getOptionRow (transpose board) inputs (boardPositions transpose board) [chr condNumber])
        return (sort options (boardPositions id board))

    3 -> lrDiagCond where
      lrDiagCond = do
        options <- if fst (head inputs) == -1 then
                      manuallySelectInputAppearence board (map snd inputs) (diagonals . reverse) "top left to bottom right diagonal" condNumber
                   else return (getOptionRow ((diagonals . reverse) board) inputs (boardPositions (diagonals . reverse) board) [chr condNumber])
        return (sort options (boardPositions id board))

    4 -> rlDiagCond where
      rlDiagCond = do
        options <- if fst (head inputs) == -1 then
                    manuallySelectInputAppearence board (map snd inputs) diagonals "bottom left to top right diagonal" condNumber
                   else return (getOptionRow (diagonals board) inputs (boardPositions diagonals board) [chr condNumber])
        return (sort options (boardPositions id board))

    5 -> boxCond where
      boxCond = do
        putStrLn "Box condition:"
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
                     manuallySelectInputAppearence board (map snd inputs) (\brd -> getBoxs brd x y) "box" condNumber
                   else return (getOptionRow ((\brd -> getBoxs brd x y) board) inputs (boardPositions (\brd -> getBoxs brd x y) board) [chr condNumber])
        return (sort options (boardPositions id board))

      getBoxs board w h = concatMap (\i -> map
        (\ j -> foldr (\ x r -> take w (drop (w * j) x) ++ r) []
        (take h (drop (h * i) board))) [0..(div (length (head board)) w - 1)]) [0..(div (length board) h - 1)]

    6 -> customCond where
      customCond = do
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
                     manuallySelectInputAppearence board (map snd inputs) shapeRows "custom shape" condNumber
                   else return (getOptionRow (shapeRows board) inputs (boardPositions shapeRows board) [chr condNumber])
        return (sort options (boardPositions id board))

    7 -> connectedInputsCond where
      connectedInputsCond = do
        inps <- if fst (head inputs) == -1 then do
            putStrLn "Tell me how often an Input is supposed to be connected with the same Input"
            getInputAppearence (map snd inputs) (\str -> "How often is " ++ str ++ " connected")
          else return inputs
        let options = connectedInputs board inps condNumber
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



replaceElem1d :: Int -> [a] -> a -> [a]
replaceElem1d i list elem = let (lxs,_:rxs) = splitAt i list in
  lxs ++ [elem] ++ rxs

replaceElem :: Int -> Int -> [[a]] -> a -> [[a]]
replaceElem a b board elem = let (lrows,xs:rrows) = splitAt a board in
  let (lxs,_:ys) = splitAt b xs in
    lrows ++ (lxs ++ [elem] ++ ys):rrows



boardPositions :: ([[String]] -> [[String]]) -> [[String]] -> [[[String]]]
boardPositions transform board = map (map (:[])) (transform (zipWith (\ i xs -> boardCols xs i 0) [0..] board))
  where
    boardCols [] _ _ = []
    boardCols (x:xs) i j = ("p" ++ show i ++ show j): boardCols xs i (j+1)

combine :: [[[String]]] -> [[String]] -> [[[String]]]
combine [] _ = []
combine (fs:fss) (n:ns) =
  foldr (\f r -> [f ++ [x] | x <- n, length f == 1 || last (splitOn "-" x) == last (splitOn "-" (f!!1)) ] ++ r) [] fs : combine fss ns

manuallySelectInputAppearence :: [[String]] -> [String] -> ([[String]] -> [[String]]) -> [Char] -> Int -> IO [[[String]]]
manuallySelectInputAppearence board inputs transform shape condNumber  = do
  putStrLn (toUpper (head shape): tail shape ++ " condition:")
  putStrLn ("One Input per " ++ shape++"? (y/n)")
  oneInput <- getLine
  if 'y' == toLower (head oneInput)
  then getConfirmation 1 board (map (1,) inputs) (getOptionRow (transform board) (map (1,) inputs) (boardPositions transform board) [chr condNumber]) condNumber
  else do
    putStrLn ("Tell me how often an Input appears (at most) per " ++ shape)
    inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the row?")
    getConfirmation 1 board inputAppearence (getOptionRow (transform board) inputAppearence (boardPositions transform board) [chr condNumber]) condNumber

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

getConfirmation :: Int -> [[String]] -> [(Int, String)] -> [[[String]]] -> Int -> IO [[[String]]]
getConfirmation i board inputs formatFunc condNumber = do
  putStrLn (foldr (\(i,input) r -> "Input " ++ input ++ " appears " ++ show i ++ " times\n" ++ r) "" inputs)
  putStrLn "Are you sure about this? (y/n)"
  oneInput <- getLine
  if 'y' == toLower (head oneInput)
  then return formatFunc
  else getSymbolicOptions i board (map (-1,) (map snd inputs)) condNumber

getOptionRow :: [[String]] -> [(Int, String)] -> [[[String]]] -> String -> [[[String]]]
getOptionRow board inputs pos lt =
  let options = map (oneInputFormat board) inputs in
  combine (map (:[]) (concat pos)) (concatItems options [])
    where
    oneInputFormat board (i,input) = let inputAmount = map (\xs -> i - length (filter (==input) xs)) board in
      let itemDigits = scanl (+) 0 inputAmount in
        concatMap (\(j,xs) -> map (\x -> if not (null x) then [] else
          -- If no input of this cond can be put unto x, then no input in general can go into this field (this signifies the ["X"])
          -- All the fields get deleted because of combines 'splitOn "-"'
          if null [(itemDigits!!j)..(itemDigits!!(j+1)-1)] then ["X"] else
              map (\k -> lt ++ show k ++ "-" ++ input) [(itemDigits!!j)..(itemDigits!!(j+1)-1)]) xs) (zip [0..] board)

    concatItems (o:opts) [] = concatItems opts o
    concatItems [] result = result
    concatItems (o:opts) result = concatItems opts (zipWith (++) result o)


getItems :: [[String]] -> Int ->  ([String], [String])
getItems opts i  = let condLetters = sort (tail (nub (map (take 1) (concat opts)))) in
  let items = map (\lt -> nub (filter (\x -> lt == take 1 x) (concat opts))) (condLetters ++ ["p"]) in
  let (pr, sr) = splitAt i items in
  (concat pr, concat sr)

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


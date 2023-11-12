{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use guards" #-}

module DlxFormat where
import Data.Char
import Data.List.Split
import Data.List
import Control.Monad


{-
todo 
7 und 8

-}

getSymbolicFormat :: [Int] -> [[String]] -> [String] -> IO [[String]]
getSymbolicFormat conds board inputs = do
  putStrLn "Let's go deeper into the selected conditions\n"
  let condLetterNumber = 96
  options <- getSymbolicFormatHelper conds board inputs (map (:[]) (concat (boardPositions board 0))) condLetterNumber
  return (filter (\x-> length x > 2 && notElem "X" x) (concat options)) where

    getSymbolicFormatHelper [] _ _ format condLetterNumber = return format
    getSymbolicFormatHelper (c:conds) board inputs format condLetterNumber = do

      options <- getSymbolicOptions c board inputs (condLetterNumber+1)
      getSymbolicFormatHelper conds board inputs (combine format (removePos options)) (condLetterNumber+1)

    removePos :: [[[String]]] -> [[String]]
    removePos = map (foldr (\x r -> tail x ++ r) [])

getSymbolicOptions :: Int -> [[String]] -> [String] -> Int -> IO [[[String]]]
getSymbolicOptions cond board inputs condNumber =
  case cond of
    1 -> rowCond where
      rowCond = do
        putStrLn "Row condition:"
        putStrLn "One Input per row? (y/n)"
        oneInput <- getLine
        if 'y' == toLower (head oneInput)
        then getConfirmation 1 board (map (1,) inputs) (getOptionRow board (map (1,) inputs) (boardPositions board 0) [chr condNumber])
        else do
          putStrLn "Tell me how often an Input appears (at most) per row"
          inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the row?")
          getConfirmation 1 board inputAppearence (getOptionRow board inputAppearence (boardPositions board 0) [chr condNumber])

    2 -> colCond where
      colCond = do
        putStrLn "Column condition:"
        putStrLn "One Input per column? (y/n)"
        oneInput <- getLine
        if 'y' == toLower (head oneInput)
        then do
          options <- getConfirmation 2 board (map (1,) inputs) (getOptionRow (transpose board) (map (1,) inputs) ( transpose (boardPositions board 0)) [chr condNumber])
          return (sort options (boardPositions board 0))
        else do
          putStrLn "Tell me how often an Input appears (at most) per column"
          inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the column?")
          options <- getConfirmation 2 board inputAppearence (getOptionRow (transpose board) inputAppearence ( transpose (boardPositions board 0)) [chr condNumber])
          return (sort options (boardPositions board 0))

    3 -> lrDiagCond where
      lrDiagCond = do
        putStrLn "Top left to bottom right diagonal condition:"
        putStrLn "One Input per diagonal? (y/n)"
        oneInput <- getLine
        if 'y' == toLower (head oneInput)
        then do
          options <- getConfirmation 3 board (map (1,) inputs) (getOptionRow (diagonals (reverse board)) (map (1,) inputs)
            (diagonals (reverse (boardPositions board 0))) [chr condNumber] )
          return (sort options (boardPositions board 0))
        else do
          putStrLn "Tell me how often an Input appears (at most) per diagonal"
          inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the diagonal?")
          options <- getConfirmation 3 board inputAppearence (getOptionRow  (diagonals (reverse board)) inputAppearence
            (diagonals (reverse (boardPositions board 0))) [chr condNumber] )
          return (sort options (boardPositions board 0))

    4 -> rlDiagCond where
      rlDiagCond = do
        putStrLn "Top right to bottom left diagonal condition:"
        putStrLn "One Input per diagonal? (y/n)"
        oneInput <- getLine
        if 'y' == toLower (head oneInput)
        then do
          options <- getConfirmation 4 board (map (1,) inputs) (getOptionRow (diagonals board) (map (1,) inputs)
            (diagonals (boardPositions board 0)) [chr condNumber])
          return (sort options (boardPositions board 0))
        else do
          putStrLn "Tell me how often an Input appears (at most) per diagonal"
          inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the diagonal?")
          options <- getConfirmation 4 board inputAppearence (getOptionRow  (diagonals board) inputAppearence
            (diagonals (boardPositions board 0)) [chr condNumber])
          return (sort options (boardPositions board 0))

    5 -> boxCond where
      boxCond = do
        putStrLn "Box condition:"
        putStrLn "How big is the box? \nTell me the width of the box:"
        widthStr <- getLine
        let width = read widthStr :: Int
        putStrLn "Tell me the height of the box:"
        heightStr <- getLine
        let height = read heightStr :: Int
        putStrLn "One Input per box? (y/n)"
        oneInput <- getLine
        if 'y' == toLower (head oneInput)
        then do
          options <- getConfirmation 5 board (map (1,) inputs) (getOptionRow (getBoxs board width height) (map (1,) inputs)
            (getBoxs (boardPositions board 0) width height) [chr condNumber])
          return (sort options (boardPositions board 0))
        else do
          putStrLn "Tell me how often an Input appears (at most) per box"
          inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the box?")
          options <- getConfirmation 5 board inputAppearence (getOptionRow  (getBoxs board width height) inputAppearence
            (getBoxs (boardPositions board 0) width height) [chr condNumber])
          return (sort options (boardPositions board 0))

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

        putStrLn "One Input per shape? (y/n)"
        oneInput <- getLine
        if 'y' == toLower (head oneInput)
        then do
          options <- getConfirmation 6 board (map (1,) inputs) (getOptionRow (shapeRows board) (map (1,) inputs)
            (shapeRows (boardPositions board 0)) [chr condNumber] )
          return (sort options (boardPositions board 0))
        else do
          putStrLn "Tell me how often an Input appears (at most) per shape"
          inputAppearence <- getInputAppearence inputs (\str -> "How often appears '" ++ str ++ "' in the shape?")
          options <- getConfirmation 6 board inputAppearence (getOptionRow  (shapeRows board) inputAppearence
            (shapeRows (boardPositions board 0)) [chr condNumber] )
          return (sort options (boardPositions board 0))

    7 -> connectedInputsCond where
      connectedInputsCond = do
        putStrLn "Tell me how often an Input is supposed to be connected with the same Input"
        inputAppearence <- getInputAppearence inputs (\str -> "How often is " ++ str ++ " connected")
        let options = filomino inputAppearence
        error (show options)
        return []
        where
          filomino :: [(Int, String)] -> [[String]]
          filomino inputs = do
            let dis = [0..length board-1]
            let djs = [0..length (head board)-1]
            foldr (\input r3 -> foldr (\di r2 -> 
              foldr (\dj r1 -> checkEdges input di dj ++ r1) [] djs ++ r2) [] dis ++ r3) [] inputs

          checkEdges :: (Int, String) -> Int -> Int -> [[String]]
          checkEdges (occur,input) di dj
            | board!!di!!dj == "" = if not ( dj>0 && checkVertex di (dj-1) || di>0 && checkVertex (di-1) dj ||
                dj<length (head board) -1 && checkVertex di (dj+1) || di<length board - 1 && checkVertex (di+1) dj )
              then getOptions (occur,input) di dj 0
              else []
            | not (checkVertex di dj) = []
            | otherwise = getOptions (occur,input) di dj 1
            where
              checkVertex i j = board !! i !! j == input
          getOptions (occur,input) di dj forcing =
            let options = r1 in
              options

            where
            r1 = let tag = replicate (length board) (replicate (length (head board)) 0) in
              let tagNew = replaceElem di dj tag 1 in
              let vv = (di,dj) : [(-1,-1) | x <- [0..(length board * length (head board))]] in
              let ii = 0 : [-1 | x <- [0..(length board * length (head board))]] in
              let aa = 0 : [-1 | x <- [0..(length board * length (head board))]] in
              let v = 0 in
              let a = 0 in
              let i = 0 in
              let l = 1 in
                r2 [] tagNew vv v ii i aa a l
            r2 :: [[String]] -> [[Int]] -> [(Int,Int)] -> Int -> [Int] -> Int -> [Int] -> Int -> Int -> [[String]]
            r2 options tag vv v ii i aa a l  =
              if forcing/= 0 then
                let stack = (board!!fst (head vv)!!snd (head vv)) : ["" | x <- [0..(length board * length (head board))]] in
                let (tag', vv', ii', aa', l') = dfs (tag,vv,ii,aa,l,stack,1) in
                if null tag' then r7 options tag vv v ii i aa a l -- problem goto r7 
                else r2Helper options tag' vv' v ii' i aa' a l'
              else r2Helper options tag vv v ii i aa a l
              where
              r2Helper options tag vv v ii i aa a l =
                if l == occur then let (tag', vv', aa', l') = undoForcedMove l aa vv tag in
                  if null tag' then printOptions vv : options else -- DONE
                    r3 (printOptions vv : options) tag' vv' v ii i aa' a l'
                else r3 options tag vv v ii i aa a l

              dfs (tag,vv,ii,aa,l,stack,s) = if s == 0 then (tag,vv,ii,aa,l) else
                --Falls index error <--- hier das problem wahrscheinlich
                let s' = s-1 in
                let u = stack!!s' in
                let north@(tagn,_,_,_,_,_,_) = dfsHelper (tag,vv,ii,aa,l,stack,s') (di-1) dj in
                if null tagn then ([], vv, ii, aa, l) else
                let east@(tage,_,_,_,_,_,_) = dfsHelper north di (dj+1) in
                if null tage then ([], vv, ii, aa, l) else
                let south@(tags,_,_,_,_,_,_) = dfsHelper east (di+1) dj in
                if null tags then ([], vv, ii, aa, l) else
                let west@(tagw,_,_,_,_,_,_) = dfsHelper south  di (dj-1) in
                if null tagw then ([], vv, ii, aa, l) else
                  dfs west


              dfsHelper old@(tag,vv,ii,aa,l,stack,s) i j
                | i<0 || i == length board  || j<0 || j == length (head board) = old
                | board!!i!!j /= input || tag!!i!!j /= 0 = old
                | (i < di || i == di && j < dj) || l == occur = ([],vv,ii,aa,l,stack,s)
                | otherwise = let l' = l+1 in
                  let s' = s+1 in
                    (replaceElem i j tag 1, replaceElem1d l' vv (i,j)
                    , ii, replaceElem1d l aa 0, l', replaceElem1d s' stack (board!!i!!j), s')

              printOptions vv = let stamp = map (\i -> map (\j -> (i,j) `elem` vv)
                                      [0..length (head board)-1 ]) [0..length board-1] in
                let primary = map (\(i,j) -> show i ++ show j) (filter (\v -> fst v /= -1) vv) in
                    primary ++ concatMap (\(i,j) -> if board!!i!!j == "" then
                      let north = printOptionsHelper stamp (i-1) j "north" in
                      let east = printOptionsHelper stamp i (j+1) "east" in
                      let south = printOptionsHelper stamp (i+1) j "south" in
                      let west = printOptionsHelper stamp i (j-1) "west" in
                          north ++ east ++ south ++ west else []) (filter (\v -> fst v /= -1) vv)

              printOptionsHelper stamp i j str
                | i<0 || i == length board  || j<0 || j == length (head board) = []
                | not (stamp!!i!!j) = []
                | str == "north" = ["h" ++ show (i+1) ++ show j ++ input]
                | str == "east" = ["v" ++ show i ++ show j ++ input]
                | str == "south" = ["h" ++ show i ++ show j ++ input]
                | str == "west" = ["v" ++ show i ++ show (j+1) ++ input]

              undoForcedMove l aa vv tag =
                if aa!!l /= 0 then (tag, vv, aa, l) else
                  if l == 0 then ([], vv, aa, l) --goto done aka options
                else let (i,j) = vv!!l in
                  undoForcedMove l aa vv (replaceElem i j tag 0)
            r3 options tag vv v ii i aa a l = let aa' = replaceElem1d a aa (aa!!a + 1) in
              r4 options tag vv v aa' a ii i l

            r4 options tag vv v aa a ii i l =
              let ii' = replaceElem1d i ii (ii!!i + 1) in
              let aa' = replaceElem1d a aa 1 in
              if aa!!a /= 5 then r5 options tag vv v aa a ii i l
              else if i == l-1 then r6 options tag vv v aa a ii i l
              else r5 options tag vv v aa' a ii' i l

            r5 options tag vv v aa a ii i l =
              let (n, m) = getDirection in
              if not (n >= 0 && n < length board && m >= 0 && m < length (head board)) || board!!n!!m == ""
                then r3 options tag vv v aa a ii i l
                else (let tag' = replaceElem n m tag (tag!!n!!m +1) in
                  if tag'!!n!!m > 1 then r3 options tag' vv v aa a ii i l
                  else if forcing == 0 then r5Helper options tag' vv v aa a ii i l n m
                  else r5ToR2 options tag' vv v aa a ii i l n m )
                    where
              r5ToR2 options tag vv v aa a ii i l n m =
                let ii' = replaceElem1d l ii (ii!!i) in
                let aa' = replaceElem1d l aa (aa!!a) in
                let vv' = replaceElem1d l vv (n,m) in
                  r2 options tag vv' v aa' a ii' i (l+1)

              r5Helper options tag vv v aa a ii i l n m
                | n < di || n == di && m < dj = r3 options tag vv v aa a ii i l
                | n > 0 && board!!(n-1)!!m == input ||
                m > 0 && board!!n!!(m-1) == input ||
                n < length board -1 && board!!(n+1)!!m == input ||
                m < length (head board) -1 && board!!n!!(m+1) == input = r3 options tag vv v aa a ii i l
                | otherwise = r5ToR2 options tag vv v aa a ii i l n m
              getDirection = let (i,j) = vv!!v in
                case aa!!a of
                  1 -> (i-1,j)
                  2 -> (i,j+1)
                  3 -> (i+1,j)
                  4 -> (i,j-1)
                  otherwise -> error (show (aa!!a))

            r6 options tag vv v aa a ii i l =
              let (tag1, l') = r6Helper tag l in
              if l' == 0 then options else
              let i' = l' in
              let k = ii!!i'+1 in
              let tag2 = foldr (\x r -> let (n,m) = vv!!x in r6Untag r n m ) tag [k..l'] in
              let aa' = replaceElem1d l aa (aa!!l + 1) in
              let a' = l' in
              let v' = i in
              let tag3 = foldr (\x r -> let (n,m) = vv!!x in r6Untag r n m ) tag2 [aa'!!l' .. 4] in
              r3 options tag3 vv v' aa a ii i' l'
                where
                r6Helper tag l =
                  if l == 0 || aa!!l == 0 then (tag, l) else
                    let l' = l-1 in
                    let (n,m) = vv!!l' in
                    r6Helper (r6Untag tag n m) l'

                r6Untag tag n m =
                  if n >= 0 && n < length board && m >= 0 &&
                    m < length (head board) && board!!n!!m == ""
                  then replaceElem n m tag (tag!!n!!m -1)
                  else tag

            r7 options tag vv v aa a ii i l =
              r3 options tag vv i aa l ii l l

            checkVertex i j = board !! i !! j == input

    8 -> noEdgeCond where
        noEdgeCond = return []
    where

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

  getConfirmation :: Int -> [[String]] -> [(Int, String)] -> [[[String]]] -> IO [[[String]]]
  getConfirmation i board inputs formatFunc = do
    putStrLn (foldr (\(i,input) r -> "Input " ++ input ++ " appears " ++ show i ++ " times\n" ++ r) "" inputs)
    putStrLn "Are you sure about this? (y/n)"
    oneInput <- getLine
    if 'y' == toLower (head oneInput)
    then return formatFunc
    else getSymbolicOptions i board (map snd inputs) condNumber

  getOptionRow :: [[String]] -> [(Int, String)] -> [[[String]]] -> String -> [[[String]]]
  getOptionRow board inputs pos lt =
    let options = map (oneInputFormat board) inputs in
    combine (map (:[]) (concat pos)) (concatItems options [])
      where
      oneInputFormat board (i,input) = let inputAmount = map (\xs -> i - length (filter (==input) xs)) board in
        let itemDigits = scanl (+) 0 inputAmount in
          concatMap (\(j,xs) -> map (\x -> if not (null x) then [] else
            -- If no input of this cond can be put onto x, then no input in genral can go onto this field (this signifies the ["X"])
            -- All the fields get deleted because of combines 'splitOn "-"'
            if null [(itemDigits!!j)..(itemDigits!!(j+1)-1)] then ["X"] else
                map (\k -> lt ++ show k ++ "-" ++ input) [(itemDigits!!j)..(itemDigits!!(j+1)-1)]) xs) (zip [0..] board)

      concatItems (o:opts) [] = concatItems opts o
      concatItems [] result = result
      concatItems (o:opts) result = concatItems opts (zipWith (++) result o)

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


boardPositions :: [[String]] -> Int -> [[[String]]]
boardPositions [] _ = []
boardPositions (xs:xss) i = boardCols xs i 0 : boardPositions xss (i+1)
  where
    boardCols [] _ _ = []
    boardCols (x:xs) i j = ["p" ++ show i ++ show j]: boardCols xs i (j+1)

combine :: [[[String]]] -> [[String]] -> [[[String]]]
combine [] _ = []
combine (fs:fss) (n:ns) = if null n then fs:combine fss ns else
  foldr (\f r -> [f ++ [x] | x <- n, length f == 1 || last (splitOn "-" x) == last (splitOn "-" (f!!1)) ] ++ r) [] fs : combine fss ns


--
getItems :: [[String]] -> Int -> ([String], [String])
getItems opts i = let condLetters = sort (tail (nub (map (take 1) (concat opts)))) in
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

test = getSymbolicOptions 1 [["","","Q"],["","",""],["","",""]] ["Q"]
test1 = getSymbolicOptions 2 [["","","Q"],["","",""],["","",""]] ["Q"]
test2 = getSymbolicOptions 3 [["" | x<-[1..4]] | y<-[1..4]] ["Q"]

test3 = getSymbolicFormat [1,2] [["","","Q",""],["","","",""],["","","",""],["","","",""]] ["Q"]

test4 = getSymbolicOptions 6 (([show x | x<-[1..7]]) : [[show (x+9*y) | x<-[1..9]] | y<-[1..8]]) ["Q"]



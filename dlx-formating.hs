import Dlx
import Queens
import Sudoku 
import Data.Char
import Data.List
import Data.List.Split
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
-- Then provCombine them in some way that generates option
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
rowCond board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs )) board)

colCond :: [[String]] -> Bool 
colCond board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs)) (transpose board))

lrDiag :: [[String]] -> Bool 
lrDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs)) (diagonals board))

rlDiag :: [[String]] -> Bool 
rlDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "" then 1 else 0) xs)) (diagonals (reverse board)))

rowCondSudoku board= let inputs = [show x | x <- [1..9]] in 
    all (==True) (map (all (<=1)) (map (\input -> map (\xs -> sum (map (\x -> if x == input then 1 else 0) xs)) board ) inputs))

colCondSudoku board= let inputs = [show x | x <- [1..9]] in 
    all (==True) (map (all (<=1)) (map (\input -> map (\xs -> sum (map (\x -> if x == input then 1 else 0) xs)) (transpose board) ) inputs))

condTest = rowCondSudoku [["1","2","3"], ["1","1","2","2","2","3","4"]]
condTest2 = rowCondSudoku [[show x | x <- [1..9]], reverse [show x | x <- [1..9]]]

help = [["" | y <- [1..9]] | x <-[1..9]]

-- Testing if dlxFormat can output the right queens format based on only the conditions 
dlxFormatTest = let (items, options) = dlxFormat ([rowCond, colCond],[lrDiag, rlDiag]) [[""| x <- [0..3]] | y <- [0..3]] ["Q"] in
    options 

dlxFormatTest2 = let (items, options) = dlxFormat ([rowCondSudoku, colCondSudoku],[]) sudokuPuzzle [show x | x <- [1..9]] in 
    options
sudokuPuzzle = [["","","","2","6","","7","","1"],["6","8","","","7","","","9",""],
    ["1","9","","","","4","5","",""],["8","2","","1","","","","4",""],["","","4","6","","2","9","",""],
    ["","5","","","","3","","2","8"],["","","9","3","","","","7","4"],["","4","","","5","","","3","6"],["7","","3","","1","8","","",""]]
--["...26.7.1","68..7..9.","19...45","82.1...4","..46.29",".5...3.28","..93...74",".4..5..36","7.3.18"]
test = [x | x <- []]
-- get dlx format based on conditions and board 
dlxFormat (primaryCond, secondaryCond) board inputs = let boardPos = boardPositions board 0 in 
    let options = nextCond (primaryCond ++ secondaryCond) inputs (map (map concat) boardPos) boardPos 0 board in 
    let (primary, secondary) = findItems options (length primaryCond) in 
        ((concat primary, concat secondary),  filter (\x -> 1 < length x) (map concat options)) where
        
        boardPositions :: [[String]] -> Int -> [[[String]]]
        boardPositions [] _ = []
        boardPositions (xs:xss) i = boardCols xs i 0 ++ boardPositions xss (i+1)
            where
                boardCols [] _ _ = []
                boardCols (x:xs) i j = [["p" ++ show i ++ show j]]: boardCols xs i (j+1)

        findItems opts i = let options = map nub (transpose  opts) in
            let (pos, rest) = splitAt 1 options in
            let (pr, sr) = splitAt i rest in 
                (pr, sr++pos)
        
        nextCond [] _ _ format _ _ = format 
        -- searches for next conditions format, provCombines it with the previous format, and saves it in format for the next function call
        nextCond (c:cs) inputs allpos format i board = 
            nextCond cs inputs allpos ( combine format (nextPos c inputs allpos [] [] 0 [condLetter i] board)) (i+1) board

        condLetter i = chr (97+i)


        combine :: [[[String]]] -> [[String]] -> [[[String]]]    
        combine  _ [] = []
        combine (fs:fss) (n:ns) = if n == [] then fs:combine fss ns else 
            map (\f -> [f ++ [x] | x <- n, length f == 1 || last(splitOn "-" x) == last (splitOn "-" (f!!1))]) fs ++ combine fss ns  

        nextPos :: ([[String]] -> Bool)-> [String]-> [[String]]-> [[String]] -> [String] -> Int -> String -> [[String]] -> [[String]]
        nextPos cond inputs allpos provFormat store i lt board = if length allpos == length provFormat then provFormat else
            let newOptions = testNewInputs cond inputs allpos provFormat i lt board in 
                if newOptions /= []
                    then nextPos cond inputs allpos (provFormat ++ [newOptions]) (newOptions ++ store) (i+1) lt board
                    else let newOldOptions = testOldPos cond allpos provFormat store board in
                        nextPos cond inputs allpos (provFormat ++ [newOldOptions]) store i lt board  

        testNewInputs _ [] _ _ _ _ _ = []
        testNewInputs cond (inp:inputs) allpos provFormat i lt board = let newItem = lt ++ show i ++ "-" ++ inp in
            let provFormat2 = moveToTop provFormat inp  in
                --if length provFormat >6 && newItem == "a1-8" then error (show (test cond allpos (provFormat2 ++ [[newItem]]) board)) else
                    if test cond allpos (provFormat2 ++ [[newItem]]) board
                    then newItem: testNewInputs cond inputs allpos provFormat i lt board 
                    else testNewInputs cond inputs allpos provFormat i lt board where 
                            
        moveToTop provFormat inp = map (\xs -> let it = getItem xs inp in 
            if it == "" then xs else it : (filter (/= it) xs)) provFormat  
        getItem xs inp = concat (filter (\x -> inp == last (splitOn "-" x)) xs)

        --Find critical option 
        testOldPos :: ([[String]] -> Bool) -> [[String]] -> [[String]] -> [String] -> [[String]] -> [String]
        testOldPos _ _ _ [] _ = []
        testOldPos cond allpos provFormat (x:xs) board = 
            let prov = moveToTop (map (filter (/=x)) provFormat) (last (splitOn "-" x)) in -- replaces x in provFormat to test if its the critical option
                --if length provFormat >6 && x == "a0-8" then error (show prov) else
                    if test cond allpos (prov ++ [[x]]) board
                    then x: testOldPos cond allpos provFormat xs board
                    else testOldPos cond allpos provFormat xs board 

        test cond allpos provFormat board = let newBoard = translate (provCombine allpos provFormat) board in 
            case newBoard of 
                Nothing -> False 
                Just b -> cond b 

        -- provisionalCombine combines the old format with new Items
        provCombine :: [[String]] -> [[String]] -> [(String, [String])]
        provCombine _ [] = []
        provCombine (p:ps) (x:xs) = (head p, x): provCombine ps xs  

--dlf := dancing links formatting
translate :: [(String, [String])] -> [[String]] -> Maybe [[String]]
translate dlf brd = let transdlf = transform dlf in  
    let bdlf = bracketRows transdlf in 
    let formatBoard = map (map (\x -> x!!1)) bdlf in 
        getBoard brd formatBoard where
      
            getBoard board formatBoard = let zipped = map (\(bs,fs) -> zipLongest bs fs "") (zipLongest board formatBoard []) in 
                if not (all (==True) (map (all (==True)) (map (map (\(b,f) -> b == "" || f == "")) zipped))) then Nothing
                    else Just (map (map (\(b,f) -> if b == "" then f else b)) zipped)  where
                        zipLongest x y sign = if length x < length y then zip (x ++ [sign | c <- [0 .. length y - length x]]) y else 
                            zip x (y ++ [sign | c <- [0 .. length x - length y]]) 

            bracketRows xss = 
                let xs = bracketRowsHelper xss in 
                let (res, _) = splitAt (length xss - length xs) xss in 
                    if xs == [] then [res] else res:bracketRows xs

            bracketRowsHelper [x] = []
            bracketRowsHelper (x1:x2:xs) = if (x1!!0)!!1 == (x2!!0)!!1 then bracketRowsHelper (x2:xs) else x2:xs

            transform [] = []
            transform ((p,x):xs) = if x == [] then [p,""]: transform xs else 
                [p, getInput (head x)]: transform (map (\(p1,x1) -> (p1, filter (/= head x) x1)) xs) 

            getInput x = let input = splitOn "-" x in last input


translateTest = translate [("p00", ["a0-Q"])] [[""| x <- [0..3]] | y <- [0..3]]

moveToTop2 provFormat inp = map (\xs -> let it = getItem xs inp in 
    if it == "" then xs else it : (filter (/= it) xs)) provFormat where 
        getItem xs inp = concat (filter (\x -> inp == last (splitOn "-" x)) xs)

moveTest = moveToTop2 [["a0-1","a0-8"],[],[],[],[],[],["a1-8"]] "8"

moveToBottom2 provFormat item = map (\xs -> if elem item xs then (filter (/=item) xs) ++ [item] else xs) provFormat 

moveTest2 = moveToBottom2 [["a0-1","a0-8"],[]] "a0-1"
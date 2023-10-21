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
rowCond board = all (==1) (map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs )) board)

colCond :: [[String]] -> Bool 
colCond board = all (==1) (map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (transpose board))

lrDiag :: [[String]] -> Bool 
lrDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (diagonals board))

rlDiag :: [[String]] -> Bool 
rlDiag board = all (<=1) (map (\xs -> sum (map (\x -> if x /= "." then 1 else 0) xs)) (diagonals (reverse board)))

rowCondSudoku board= let inputs = [show x | x <- [1..9]] in 
    all (==True) (map (all (==1)) (map (\input -> map (\xs -> sum (map (\x -> if x == input then 1 else 0) xs)) board ) inputs))

condTest = rowCondSudoku [["1","2","3"], ["1","1","2","2","2","3","4"]]
condTest2 = rowCondSudoku [[show x | x <- [1..9]], reverse [show x | x <- [1..9]]]

-- Testing if dlxFormat can output the right queens format based on only the conditions 
dlxFormatTest = let (items, options) = dlxFormat ([rowCond, colCond ],[lrDiag, rlDiag]) [["."| x <- [0..3]] | y <- [0..3]] ["Q"] in
    options 

dlxFormatTest2 = let (items, options) = dlxFormat ([rowCondSudoku],[]) [["1","2","3",".","5","6","7","8","."]] [show x | x <- [1..9]] in 
    options

-- get dlx format based on conditions and board 
dlxFormat (primaryCond, secondaryCond) board inputs = let boardPos = boardPositions board 0 in 
    let options = nextCond (primaryCond ++ secondaryCond) inputs boardPos boardPos 0 board in 
    let (primary, secondary) = findItems options (length primaryCond) in 
        ((concat primary, concat secondary),options) where
            
        boardPositions [] _ = []
        boardPositions (xs:xss) i = boardCols xs i 0 ++ boardPositions xss (i+1)
            where
                boardCols [] _ _ = []
                boardCols (x:xs) i j = ["p" ++ show i ++ show j]: boardCols xs i (j+1)

        findItems opts i = let options = map nub (transpose  opts) in
            let (pos, rest) = splitAt 1 options in
            let (pr, sr) = splitAt i rest in 
                (pr, sr++pos)
        
        nextCond [] _ _ format _ _ = format 
        -- searches for next conditions format, combines it with the previous format, and saves it in format for the next function call
        nextCond (c:cs) inputs allpos format i board = 
            nextCond cs inputs allpos (combine format (nextPos c inputs allpos [] [] 0 [condLetter i] board)) (i+1) board

        condLetter i = chr (97+i)

        nextPos cond inputs allpos provFormat store i lt board = if length allpos == length provFormat then provFormat else 
            let newOptions = testNewInputs cond inputs allpos provFormat i lt board in 
                if newOptions /= []
                    then nextPos cond inputs allpos (provFormat ++ newOptions) (newOptions ++ store) (i+1) lt board
                    else nextPos cond inputs allpos (testOldPos cond allpos provFormat store board) store i lt board  

        testNewInputs _ [] _ _ _ _ _ = []
        testNewInputs cond (inp:inputs) allpos provFormat i lt board = let newItem = lt ++ show i ++ "-" ++ inp in
            if test cond allpos (provFormat ++ [newItem]) board
                then newItem: testNewInputs cond inputs allpos provFormat i lt board 
                else testNewInputs cond inputs allpos provFormat i lt board

        --Find critical option 
        testOldPos _ _ f [] _ = error (show f)
        testOldPos cond allpos provFormat (x:xs) board = 
            let prov = map (\y -> if y == x then "." else y) provFormat in -- replaces x in provFormat to test if its the critical option
                if test cond allpos (prov ++ [x]) board
                then provFormat ++ [x] 
                else testOldPos cond allpos provFormat xs board

        test cond allpos provFormat board = let newBoard = translate (combine allpos provFormat) board in 
            case newBoard of 
                Nothing -> False 
                Just b -> cond b 

        -- combines the old format with new Items
        combine _ [] = []
        combine (p:ps) (x:xs) = (p ++ [x]): combine ps xs  

--dlf := dancing links formatting
translate :: [[String]] -> [[String]] -> Maybe [[String]]
translate dlf brd = let transdlf = transform [] dlf in  
    let bdlf = bracketRows transdlf in 
    let formatBoard = map (map (\x -> x!!1)) bdlf in 
        getBoard brd formatBoard where
      
            getBoard board formatBoard = let zipped = map (\(bs,fs) -> zip bs fs) (zip board formatBoard) in 
                if not (all (==True) (map (all (==True)) (map (map (\(b,f) -> b == "." || f == ".")) zipped))) then Nothing
                    else Just (map (map (\(b,f) -> if b == "." then f else b)) zipped)  

            bracketRows xss = 
                let xs = bracketRowsHelper xss in 
                let (res, _) = splitAt (length xss - length xs) xss in 
                    if xs == [] then [res] else res:bracketRows xs

            bracketRowsHelper [x] = []
            bracketRowsHelper (x1:x2:xs) = if (x1!!0)!!1 == (x2!!0)!!1 then bracketRowsHelper (x2:xs) else x2:xs

            transform _ [] = []
            transform store (x:xs) = if elem (x!!1) store || (x!!1) == "." then [x!!0,"."]: transform store xs else 
                [x!!0, getInput x]: transform ((x!!1):store) xs

            getInput x = let input = splitOn "-" (x!!1) in input!!1 

testTranslate = translate [["p00", "a0-Q"],["p01","a0-Q"]] [["-",".","-","-"],["-","-","-","-"]]

translateFormat dlf = let transdlf = transform [] dlf in  
    let bdlf = bracketRows transdlf in 
    map (map (\x -> x!!1)) bdlf where
        bracketRows xss = 
            let xs = bracketRowsHelper xss in 
            let (res, _) = splitAt (length xss - length xs) xss in 
                if xs == [] then [res] else res:bracketRows xs

        bracketRowsHelper [x] = []
        bracketRowsHelper (x1:x2:xs) = if (x1!!0)!!1 == (x2!!0)!!1 then bracketRowsHelper (x2:xs) else x2:xs

        transform _ [] = []
        transform store (x:xs) = if elem (x!!1) store || (x!!1) == "." then [x!!0,"."]: transform store xs else 
            [x!!0, getInput x]: transform ((x!!1):store) xs

        getInput x = let input = splitOn "-" (x!!1) in input!!1 
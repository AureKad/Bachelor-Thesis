module DlxFormating (dlxFormat, interpret) where
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

-}

-- get dlx format based on conditions, board and inputs
dlxFormat :: ([[[String]] -> Bool], [[[String]] -> Bool]) -> [[String]] -> [String] -> (([String], [String]), [[String]])
dlxFormat (primaryCond, secondaryCond) board inputs = let boardPos = boardPositions board 0 in 
                -- removes all the options that have only the position as the item
    let options = filter (\x -> length x /= 1) (concat (nextCond (primaryCond ++ secondaryCond) inputs (map (map concat) boardPos) boardPos 0 board)) in 
    let (primary, secondary) = findItems options (length primaryCond) in 
        ((concat primary, concat secondary),  options) where
        
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
            let combined = filter (/=[]) (combine format (nextPos c inputs allpos [] [] 0 [condLetter i] board)) in
            nextCond cs inputs allpos combined (i+1) board

        condLetter i = chr (97+i)


        combine :: [[[String]]] -> [[String]] -> [[[String]]]    
        combine  _ [] = []
        combine (fs:fss) (n:ns) = if n == [] then fs:combine fss ns else 
            foldr (\f r -> [f ++ [x] | x <- n, length f == 1 || last(splitOn "-" x) == last (splitOn "-" (f!!1))] ++ r) [] fs : combine fss ns  

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

interpret :: [[String]] -> [[[String]]] -> String
interpret board solutions = let result = map (\x -> insertRow (transform x) board 0) solutions in
        foldr (\x r -> printSudoku x ++ r) "" result where

        printSudoku = foldr (\xs r -> (foldr (\x r2 -> if x == "" then "- " ++ r2 else x ++ " " ++ r2) "" xs )++ "\n" ++ r) "\n"

        insertRow _ [] _ = []
        insertRow ps (xs:xss) i = let (psUpdate, row) = insertCol ps xs i 0 [] in 
            row: insertRow psUpdate xss (i+1)

        insertCol ps [] _ _ row = (ps, row)
        insertCol [] (x:xs) i j row = insertCol [] xs i (j+1) (row ++ [x])
        insertCol ((p,inp):ps) (x:xs) i j row = 
            if ("p" ++ show i ++ show j) == p then insertCol ps xs i (j+1) (row ++ [inp]) else 
                insertCol ((p,inp):ps) xs i (j+1) (row ++ [x])

        transform =  map (\x -> (head x, getInput (x!!1))) 

        getInput x = let input = splitOn "-" x in last input

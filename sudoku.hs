module Sudoku (sudoku, interpretSudoku) where 
import Data.Array.ST
import Control.Monad.ST
import Data.Array.Base
import Data.Char 
sudoku :: [String] -> (([String], [a]), [[String]])
sudoku board = let filled = fill board in 
    let (pos, row, col, box) = check (create9x9,create9x9,create9x9,create9x9) filled 0 in 
    let primary = items (pos, row, col, box) in  
    let options =  getOptions (pos, row, col, box) 0 in 
        ((primary,[]), options)

interpretSudoku :: [String] -> [[[String]]] -> String
interpretSudoku board res = 
    let bracketedRes = map bracketRows res in 
    let brd = transform board in 
        foldr (\x r -> printSudoku x ++ r) "" (map (help brd) bracketedRes) where

            printSudoku = foldr (\x r -> concat(map (\c -> c : " ") x) ++ "\n" ++ r) "\n"   

            help brd rows = let rws = map (\x -> ["p_/","___"]:x) rows in
                let res = map slotIn (zip brd rws) in 
                    map (\xs -> case xs of x:ys -> ys) res --removes the underscore

            slotIn (brd,[x]) = (x!!1)!!2: brd 
            slotIn (brd,x1:x2:xs) = 
                let diff = ord ((x2!!0)!!2) - ord ((x1!!0)!!2) in 
                let (b1, rest) = splitAt (diff-1) brd in 
                   ((x1!!1)!!2 : b1) ++ slotIn (rest, x2:xs)

            transform = map (\xs -> [x | x <- xs, isDigit x && x /= '0']) 

            bracketRows xss = 
                let xs = bracketRowsHelper xss in 
                let (res, _) = splitAt (length xss - length xs) xss in 
                    if xs == [] then [res] else res:bracketRows xs  

            bracketRowsHelper [x] = []
            bracketRowsHelper (x1:x2:xs) = if (x1!!0)!!1 == (x2!!0)!!1 then bracketRowsHelper (x2:xs) else x2:xs
        

     
fill :: [String] -> [String]
fill board = if length board /= 9 then error "The board length is not 9" else 
    fill_rows board where 
        fill_rows board = map frow board 
 
        frow row = if length row > 9 then error "Too many items in row " else 
            row ++ ['_' | x <- [1 .. (9 - length row)]]

create9x9 :: [[Int]]
create9x9 = [[ 0 | x <- [1..9]] | y <- [1..9]]

check (pos,row,col,box) _ 9 = (pos,row,col,box)
check (pos,row,col,box) board j = check (innerLoop (pos,row,col,box) board j 0) board (j+1) where 

    innerLoop (pos,row,col,box) _ j 9 = (pos,row,col,box)
    innerLoop (pos,row,col,box) board j k = 
        if not (isDigit ((board!!j)!!k) && (board!!j)!!k /= '0') then innerLoop (pos,row,col,box) board j (k+1) else 
            let (d,x) = (digitToInt ((board!!j)!!k) - 1, boxx j k) in  
                -- position update, checks row for duplicates, checks columns, checks box
                innerLoop (update pos j k (d+1), checkArr row j k d "columns" "row", 
                            checkArr col k j d "rows" "column", checkArr box x j d "rows" "box") board j (k+1) 

    checkArr arr j k d errMsg1 errMsg2 = if (arr!!j)!!d /= 0 then 
        error ("digit " ++ show (d+1) ++ " appears in " ++ errMsg1 ++ " " ++ show ((arr!!j)!!d - 1) ++ 
        " and " ++ show k ++ " of "++ errMsg2 ++ " " ++ show j ++ "!") else
            update arr j d (k+1)      

items (pos, row, col, box) = 
    getItems pos "p" 0 ++ getItems row "r" 0 ++ getItems col "c" 0 ++ getItems box "b" 0 where 
        
        getItems arr lt 9 = [] 
        getItems arr lt j = innerLoop arr lt j 0 ++ getItems arr lt (j+1) where
            innerLoop arr lt j 9 = [] 
            innerLoop arr lt j k = if (arr!!j)!!k /= 0 then innerLoop arr lt j (k+1) else 
                if lt == "p" then (lt ++ show j ++ show k):innerLoop arr lt j (k+1) else 
                    (lt ++ show j ++ show (k+1)):innerLoop arr lt j (k+1)

getOptions (pos, row, col, box) 9 = [] 
getOptions (pos, row, col, box) j = middleLoop (pos, row, col, box) j 0 ++ getOptions (pos, row, col, box) (j+1) where 
    middleLoop (pos, row, col, box) j 9 = [] 
    middleLoop (pos, row, col, box) j k = innerLoop (pos, row, col, box) j k 0 ++ middleLoop (pos, row, col, box) j (k+1) where 
        innerLoop (pos, row, col, box) j k 9 = [] 
        innerLoop (pos, row, col, box) j k d = let x = boxx j k in  
            if  not (((pos!!j)!!k == 0) && ((row!!j)!!d == 0) && ((col!!k)!!d == 0) && ((box!!(x))!!d == 0))
                then innerLoop (pos, row, col, box) j k (d+1) else 
                    [("p" ++ show j ++ show k), ("r" ++ show j ++ show (d+1)), ("c" ++ show k ++ show (d+1)), 
                    ("b" ++ show x ++ show (d+1))]: innerLoop (pos, row, col, box) j k (d+1)

update arr j k x = let (r1,(r:r2)) = splitAt j arr in 
    let (i1,(_:i2)) = splitAt k r in 
        r1 ++ (i1 ++ x:i2):r2

boxx j k =  (div j 3)*3 + div k 3 
    
test = sudoku ["1,,2,,",",,3,",",,1,,,,,,","4,,,,,",",,,,,,5,,","","7","8","9"]

test2 = sudoku ["53,,7","6,,195",",98,,,,6","8,,,6,,,3","4,,8,3,,1","7,,,2,,,6",",6,,,,28",",,,419,,5",",,,,8,,79"]

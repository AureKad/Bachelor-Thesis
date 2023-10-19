import Dlx 
import Sudoku 
import Queens 

data Sudoku = Sudoku {
    board :: [String], 
    sinterpretResults :: Bool, 
    sshowResults :: Int -- negative Integer for all 
}

data Queens = Queens {
    queenAmount :: Int, 
    qinterpretResults :: Bool, 
    qshowResults :: Int 
}


solveSudoku :: Sudoku -> IO ()
solveSudoku sud = let (items, options) = sudoku (board sud) in
    let solutions = dlx items options in
    let result = "Number of solutions: " ++ show (length solutions) ++ "\n" in
    let trimmed = trimSols (sshowResults sud) solutions in 
        if not (sinterpretResults sud) then putStr result else
            putStr (interpretSudoku (board sud) trimmed ++ result) where 

                trimSols :: Int -> [[[String]]] -> [[[String]]]
                trimSols i sols = if length sols <= i || i < 0 then sols else
                    take i sols    
                
solveQueens :: Queens -> IO () 
solveQueens qn = let (items, options) = queens (queenAmount qn) in 
    let solutions = dlx items options in 
    let result = "Number of solutions: " ++ show (length solutions) ++ "\n" in 
    let trimmed = trimSols (qshowResults qn) solutions in 
        if not (qinterpretResults qn) then putStr result else
            putStr (interpretQueens (queenAmount qn) trimmed ++ result) where 

                trimSols :: Int -> [[[String]]] -> [[[String]]]
                trimSols i sols = if length sols <= i || i < 0 then sols else
                    take i sols  

sudokuTest = Sudoku {
    board = ["9.6.7.4.3","...4..2",".7..23.1","5.....1",".4.2.8.6","..3.....5",".3.7...5","..7..5","4.5.1.7.8"],
    sinterpretResults = True, 
    sshowResults = 1
}

queensTest = Queens {
    queenAmount = 8, 
    qinterpretResults = False, 
    qshowResults = 10
}
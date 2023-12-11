module Queens where 
import Data.Char

queens :: Int -> (([String],[String]), [[String]])
queens n = ((primary n,secondary (2*n-3)),options (n-1) n) 
  where 
    primary 0 = []
    primary i = primary (i-1) ++ ["r" ++ encode (i-1)] ++ ["c" ++ encode (i-1)] 

    secondary 0 = []
    secondary i = secondary (i-1) ++ ["a" ++ encode i] ++ ["b" ++ encode i]

    encode i = if i < 10 then show i else  
      case intToChar (i+87) of 
        Nothing -> show (i-52)
        Just c -> [c]

    intToChar i
      | i <= 122 = Just (chr i)
      | i <= 148 = Just (chr (i-58)) 
      | otherwise = Nothing

    options 0 n = option n 0 n
    options j n = options (j-1) n ++ option (n-1) j n 
    
    option 0 j n= helper 0 j n 
    option k j n = option (k-1) j n ++ helper k j n 
    helper k j n = let rc = ["r" ++ encode j] ++ ["c" ++ encode k] in 
      let a = if j+k > 0 && (j+k <= 2*n-3) then ["a" ++ encode (j+k)] else [] in 
      let b = if n-1-j+k >0 && (n-1-j+k <= 2*n-3) then ["b" ++ encode (n-1-j+k)] else [] in 
          [rc ++ a ++ b] 

interpretQueens :: Int -> [[[String]]] -> String 
interpretQueens n sols = let interpretedRows = map (map (interpretRow n)) sols in 
  foldr (\x r -> (foldr (\y l -> y ++ "\n" ++ l) "\n" x) ++ r) "" interpretedRows where 

    interpretRow n row = help n row 0 where 
      help n row i 
        | i==n = ""
        | otherwise = if charToInt ((row!!1)!!1) == i then "Q " ++ help n row (i+1) else 
                          "- " ++ help n row (i+1)

      charToInt i = ord i - 48 
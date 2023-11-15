module ConnectedInputs(connectedInputs) where 
import Data.Char

connectedInputs :: [[String]] -> [(Int, String)] -> Int -> [[String]]
connectedInputs board inputs ltNumber = do
  let dis = [0..length board-1]
  let djs = [0..length (head board)-1]
  foldr (\input r3 -> foldr (\di r2 ->
    foldr (\dj r1 -> checkEdges input di dj ++ r1) [] djs ++ r2) [] dis ++ r3) [] inputs
    where

  checkEdges ::(Int, String) -> Int -> Int -> [[String]]
  checkEdges (occur,input) di dj
    | board!!di!!dj == "" = if not ( dj>0 && checkVertex di (dj-1) || di>0 && checkVertex (di-1) dj ||
        dj<length (head board) -1 && checkVertex di (dj+1) || di<length board - 1 && checkVertex (di+1) dj )
      then getOptions (occur,input) di dj 0
      else []
    | not (checkVertex di dj) = []
    | otherwise = getOptions (occur,input) di dj 1
    where
      checkVertex i j = board !! i !! j == input

  getOptions :: (Int, String) -> Int -> Int -> Int -> [[String]]
  getOptions (occur,input) di dj forcing =
    let options = r1 in
      options

    where
    r1 :: [[String]]
    r1 = let tag = replicate (length board) (replicate (length (head board)) 0) in
      let tagNew = replaceElem di dj tag 1 in
      let vv = (di,dj) : [(-1,-1) | x <- [0..occur]] in
      let ii = 0 : [0 | x <- [0..occur]] in
      let aa = 0 : [0 | x <- [0..occur]] in
      let v = (di,dj) in
      let a = 0 in
      let i = 0 in
      let l = 1 in
        r2 [] tagNew vv v aa a ii i l

    r2 ::  [[String]] -> [[Int]] -> [(Int,Int)] -> (Int,Int) -> [Int] -> Int -> [Int] -> Int -> Int -> [[String]]
    r2 options tag vv v aa a ii i l  =
      if forcing /= 0 then
        let (x, y) = vv!!(l-1) in
        let stack = (x,y) : [(-1,-1) | x <- [0..(length board * length (head board))]] in
        let (problem, tag', vv', ii', aa', l') = dfs (tag,vv,ii,aa,l,stack,1) in
        if problem then r7 options tag' vv' v aa' a ii' i l' -- problem goto r7 
        else r2Helper options tag' vv' v aa' a ii' i l'
      else r2Helper options tag vv v aa a ii i l

      where
      r2Helper ::  [[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int)-> [Int]-> Int-> [Int]-> Int-> Int-> [[String]]
      r2Helper options tag vv v aa a ii i l =
        if l == occur then let (tag', vv', aa', l') = undoForcedMove (l-1) aa vv tag in
          if null tag' then printOptions vv : options else -- DONE
            r3 ( options ++ [printOptions vv]) tag' vv' v aa' a ii i l'
        else r3 options tag vv v aa a ii i l

      -- if adjacent cell is a d-cell check how long that d-omino is
      dfs ::([[Int]], [(Int, Int)], [Int], [Int], Int, [(Int, Int)], Int) -> (Bool,[[Int]], [(Int, Int)], [Int], [Int], Int)
      dfs (tag,vv,ii,aa,l,stack,s) = if s == 0 then (False,tag,vv,ii,aa,l) else
        let s' = s-1 in
        let (x,y) = stack!!s' in
        let north@(tagn,_,_,_,_,_,_) = dfsHelper (tag,vv,ii,aa,l,stack,s') (x-1) y in
        if null tagn then (True, tag, vv, ii, aa, l) else
        let east@(tage,vve,iie,aae,le,_,_) = dfsHelper north x (y+1) in
        if null tage then (True, tagn, vve, iie, aae, le) else
        let south@(tags,vvs,iis,aas,ls,_,_) = dfsHelper east (x+1) y in
        if null tags then (True, tage, vvs, iis, aas, ls) else
        let west@(tagw,vvw,iiw,aaw,lw,_,_) = dfsHelper south  x (y-1) in
        if null tagw then (True, tags, vvw, iiw, aaw, lw) else
          dfs west

      dfsHelper :: ([[Int]], [(Int, Int)], [Int], [Int], Int, [(Int, Int)], Int)-> Int-> Int-> ([[Int]], [(Int, Int)], [Int], [Int], Int, [(Int, Int)], Int)
      dfsHelper old@(tag,vv,ii,aa,l,stack,s) x y
        | x<0 || x >= length board  || y<0 || y >= length (head board) = old
        | board!!x!!y /= input || tag!!x!!y /= 0 = old
        | (x < di || x == di && y < dj) || l == occur = ([],vv,ii,aa,l,stack,s)
        | otherwise = let l' = l+1 in
          let s' = s+1 in
            (replaceElem x y tag 1, replaceElem1d l vv (x,y)
            , ii, replaceElem1d l aa 0, l', replaceElem1d s stack (x,y), s')
      printOptions ::  [(Int, Int)] -> [String]
      printOptions vv =
        let stamp = map (\i -> map (\j -> (i,j) `elem` vv)
                              [0..length (head board)-1 ]) [0..length board-1] in
        let primary = map (\(i,j) ->"a" ++ show i ++ "-" ++ show j) (filter (\v -> fst v /= -1) vv) in
            primary ++ concatMap (\(i,j) -> if board!!i!!j == "" then
              let north = printOptionsHelper stamp (i-1) j "north" in
              let east = printOptionsHelper stamp i (j+1) "east" in
              let south = printOptionsHelper stamp (i+1) j "south" in
              let west = printOptionsHelper stamp i (j-1) "west" in
                  north ++ east ++ south ++ west else []) (filter (\v -> fst v /= -1) vv)

      printOptionsHelper ::  [[Bool]] -> Int -> Int -> String -> [String]
      printOptionsHelper stamp i j str
        | i<0 || i == length board  || j<0 || j == length (head board) = []
        | stamp!!i!!j || board!!i!!j /= "" = []
        | str == "north" = ["h" ++ show (i+1) ++ show j ++ input]
        | str == "east" = ["v" ++ show i ++ show j ++ input]
        | str == "south" = ["h" ++ show i ++ show j ++ input]
        | str == "west" = ["v" ++ show i ++ show (j+1) ++ input]

    r3 ::  [[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int)-> [Int]-> Int-> [Int]-> Int-> Int-> [[String]]
    r3 options tag vv v aa a ii i l = let a' = a+1 in
      r4 options tag vv v aa a' ii i l
    r4 :: [[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int) -> [Int]-> Int-> [Int]-> Int-> Int-> [[String]]
    r4 options tag vv v aa a ii i l
      | a /= 5 = r5 options tag vv v aa a ii i l
      | i == l-1 = r6 options tag vv v aa a ii i l
      | otherwise = r5 options tag vv (vv!!(i+1)) aa 1 ii (i+1) l

    r5 ::  [[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int)-> [Int]-> Int-> [Int]-> Int-> Int-> [[String]]
    r5 options tag vv v aa a ii i l =
      let (n, m) = getDirection v a l in
      if not (n >= 0 && n < length board && m >= 0 && m < length (head board)) || board!!n!!m /= "" -- added left part
        then r3 options tag vv v aa a ii i l
        else (let tag' = replaceElem n m tag (tag!!n!!m +1) in
          if tag'!!n!!m > 1 then r3 options tag' vv v aa a ii i l
          else if forcing == 0 then r5Helper options tag' vv v aa a ii i l n m
            else r5ToR2 options tag' vv v aa a ii i l n m )
            where
      r5ToR2 :: [[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int)-> [Int]-> Int-> [Int]-> Int-> Int-> Int-> Int-> [[String]]
      r5ToR2 options tag vv v aa a ii i l n m =
        let ii' = replaceElem1d l ii i in
        let aa' = replaceElem1d l aa a in
        let vv' = replaceElem1d l vv (n,m) in
          r2 options tag vv' v aa' a ii' i (l+1)
      r5Helper :: [[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int)-> [Int]-> Int-> [Int]-> Int-> Int-> Int-> Int-> [[String]]
      r5Helper options tag vv v aa a ii i l n m
        | n < di || n == di && m < dj = r3 options tag vv v aa a ii i l
        | n > 0 && board!!(n-1)!!m == input ||
        m > 0 && board!!n!!(m-1) == input ||
        n < length board -1 && board!!(n+1)!!m == input ||
        m < length (head board) -1 && board!!n!!(m+1) == input = r3 options tag vv v aa a ii i l
        | otherwise = r5ToR2 options tag vv v aa a ii i l n m

    r6 ::[[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int)-> [Int]-> Int-> [Int]-> Int-> Int-> [[String]]
    r6 options tag vv v aa a ii i l =
      let (tag1, l') = r6Helper tag (l-1) in
      if null tag1 then options else
      let i' = ii!!l' in
      let k = i'+1 in
      let tag2 = foldr (\x r -> let (n,m) = vv!!x in foldr (\(x,y) r1 -> r6Untag r1 (n+x) (m+y)) r [(-1,0),(0,-1),(1,0),(0,1)]) tag1 [k..l'] in
      let a' = aa!!l' +1 in
      let v' = vv!!i' in
      let tag3 = foldr (\(x,y) r -> let (n,m) = v' in r6Untag r (n+x) (m+y) ) tag2 (reverse (take (5-a') (reverse [(-1,0),(0,-1),(1,0),(0,1)]))) in
      r3 options tag3 vv v' aa (aa!!l') ii i' l'
        where
        r6Helper :: [[Int]] -> Int -> ([[Int]], Int)
        r6Helper tag l
          | aa!!l /= 0 = (tag, l)
          | l==0 = ([],l)
          | otherwise = let (n,m) = vv!!l in
                        let tag' = r6Untag (r6Untag (r6Untag (r6Untag tag (n-1) m) n (m-1)) (n+1) m) n (m+1) in
                        r6Helper (replaceElem n m tag' 0) (l-1)
        r6Untag :: [[Int]] -> Int -> Int -> [[Int]]
        r6Untag tag n m =
          if n >= 0 && n < length board && m >= 0 &&
            m < length (head board) && board!!n!!m == ""
          then replaceElem n m tag (tag!!n!!m -1)
          else tag

    r7 :: [[String]]-> [[Int]]-> [(Int, Int)]-> (Int,Int)-> [Int]-> Int-> [Int]-> Int-> Int-> [[String]]
    r7 options tag vv v aa a ii i l =
      let (tag', vv', aa', l') = undoForcedMove (l-1) aa vv tag in
      r3 options tag' vv' (vv!!i) aa' (aa!!l') ii (ii!!l') l'

    checkVertex ::  Int -> Int -> Bool
    checkVertex i j = board !! i !! j == input

    getDirection ::  (Int,Int) -> Int -> Int -> (Int, Int)
    getDirection v a l = let (i,j) = v in
        case a of
          1 -> (i-1,j)
          2 -> (i,j-1)
          3 -> (i+1,j)
          4 -> (i,j+1)

    undoForcedMove ::  Int-> [Int]-> [(Int, Int)]-> [[Int]]-> ([[Int]], [(Int, Int)], [Int], Int)
    undoForcedMove l aa vv tag =
      if aa!!l /= 0 then (tag, vv, aa, l) else
        if l == 0 then ([], vv, aa, l) --goto done aka options
      else let (i,j) = vv!!l in
        undoForcedMove (l-1) aa vv (replaceElem i j tag 0)

replaceElem1d :: Int -> [a] -> a -> [a]
replaceElem1d i list elem = let (lxs,_:rxs) = splitAt i list in
  lxs ++ [elem] ++ rxs

replaceElem :: Int -> Int -> [[a]] -> a -> [[a]]
replaceElem a b board elem = let (lrows,xs:rrows) = splitAt a board in
  let (lxs,_:ys) = splitAt b xs in
    lrows ++ (lxs ++ [elem] ++ ys):rrows
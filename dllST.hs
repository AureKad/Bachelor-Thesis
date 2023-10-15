module DllST where
import Control.Monad.ST
import Data.STRef
--

data Dll s a = Dll {
    dllHead :: Maybe (NodeRef s a),
    dllTail :: Maybe (NodeRef s a)
} 

data DllElem s a = DllElem {
    value :: a,
    prev ::  Maybe (NodeRef s a),
    next ::  Maybe (NodeRef s a)
} 

type NodeRef s a = STRef s (DllElem s a) 


initDll :: ST s (STRef s (Dll s a))
initDll = newSTRef Dll {dllHead = Nothing, dllTail = Nothing}

append ::  a ->  STRef s (Dll s a) -> ST s ()
append v dref = do
    dll <- readSTRef dref
    case dllTail dll of 
        Nothing -> do
            elem <- newSTRef DllElem {prev = Nothing, value = v, next = Nothing}
            modifySTRef dref (\d -> d {dllHead = Just elem, dllTail = Just elem})
        Just tailRef -> do
            elem <- newSTRef DllElem {prev = Just tailRef, value = v, next = Nothing}
            modifySTRef tailRef (\d -> d {next = Just elem})
            modifySTRef dref (\d -> d {dllTail = Just elem})

makeDllArray :: [a] -> ST s (Array Int (NodeRef s a))
makeDllArray xs = do 
    ziplist <- helper (zip [0..] xs) (length xs)
    return (array (0, length xs) ziplist) where
        helper :: [(Int, a)] -> Int -> ST s [(Int, NodeRef s a)]
        helper [] l = return []
        helper ((0, v):zs) n = do
            elem <- newSTRef DllElem {prev = n-1, value = v, next = 1}
            ((0, elem):) <$> helper zs n
        helper [(i, v)] n = do 
            elem <- newSTRef DllElem {prev = i-1, value = v, next = 0}
            ((i, elem):) <$> helper [] n
        helper ((i, v):zs) n = do 
            elem <- newSTRef DllElem {prev = i-1, value = v, next = i+1}
            ((i, elem):) <$> helper zs n

dllToList :: STRef s (Dll s a) -> ST s [a]
dllToList dref = do 
    dll <- readSTRef dref
    toListElems (dllHead dll) 
    where  
        toListElems Nothing = return [] 
        toListElems (Just ref) = do 
            elem <- readSTRef ref
            (value elem:) <$> toListElems (next elem) -- <$> infix synonym for fmap

dllFromList :: [a] -> ST s (STRef s (Dll s a))
dllFromList xs = do 
    dref <- initDll
    fromListElems dref xs
    where 
        fromListElems dref [] = return dref 
        fromListElems dref (x:xs) = do 
            append x dref
            fromListElems dref xs

getFirst :: STRef s (Dll s a) -> ST s (Maybe (NodeRef s a))
getFirst dref = do 
    dll <- readSTRef dref 
    return (dllHead dll) 

getLast :: STRef s (Dll s a) -> ST s (Maybe (NodeRef s a))
getLast dref = do 
    dll <- readSTRef dref 
    return (dllTail dll)


getValue :: NodeRef s a -> ST s a
getValue ref = do 
    elem <- readSTRef ref 
    return (value elem)

getNext :: NodeRef s a -> ST s (Maybe (NodeRef s a))
getNext ref = do 
    elem <- readSTRef ref 
    return (next elem)

getPrev :: NodeRef s a -> ST s (Maybe (NodeRef s a))
getPrev ref = do 
    elem <- readSTRef ref 
    return (prev elem)

remove :: STRef s (Dll s a) -> NodeRef s a -> ST s ()
remove dref eref = do 
    dll <- readSTRef dref
    elem <- readSTRef eref
    p <- return (prev elem) 
    n <- return (next elem) 
    case dllHead dll of 
        Nothing -> error "This doubly linked list does not contain the elem"
        Just ref 
            | ref==eref -> modifySTRef dref (\d -> d {dllHead = n})
            | otherwise -> return () 
    case dllTail dll of 
        Nothing -> error "This doubly linked list does not contain the elem"
        Just ref 
            | ref==eref -> modifySTRef dref (\d -> d {dllTail = p})
            | otherwise -> return () 
    case p of 
        Nothing -> return () 
        Just ref -> modifySTRef ref (\d -> d {next = n})
    case n of 
        Nothing -> return () 
        Just ref -> modifySTRef ref (\d -> d {prev = p})

readd :: STRef s (Dll s a) -> NodeRef s a -> ST s ()
readd dref eref = do 
    dll <- readSTRef dref
    elem <- readSTRef eref
    p <- return (prev elem) 
    n <- return (next elem) 
    case p of 
        Nothing -> return () 
        Just ref -> modifySTRef ref (\d -> d {next = Just eref})
    case n of 
        Nothing -> return () 
        Just ref -> modifySTRef ref (\d -> d {prev = Just eref})
    case dllHead dll of 
        Nothing -> modifySTRef dref (\d -> d {dllHead = Just eref})
        Just ref 
            | (Just ref) == n -> modifySTRef dref (\d -> d {dllHead = Just eref})
            | otherwise -> return () 
    case dllTail dll of 
        Nothing -> modifySTRef dref (\d -> d {dllTail = Just eref})
        Just ref 
            | (Just ref) == p -> modifySTRef dref (\d -> d {dllTail = Just eref})
            | otherwise -> return () 



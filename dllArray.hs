{-
This module contains Algorithms 2 and 3 that are described in greater Detail in Chapter 4.1.1
These algorithms constructs the data structures necessary for DLX to work (as such it is used in dlx.hs)
-}
module DllArray where
import Data.Array.ST
import Control.Monad.ST
import Data.Array.Base
import Debug.Trace

data Item a = Item {
  name :: a,
  prev ::  Int,
  next ::  Int
} deriving Show

data Node = Node {
  itm :: Int,
  up :: Int,
  down :: Int
} deriving Show

instance Eq Node where
  n1 == n2 = itm n1 == itm n2 && up n1 == up n2 && down n1 == down n2
  n1 /= n2 = itm n1 /= itm n2 || up n1 /= up n2 || down n1 /= down n2

{-
Description: Constructs the item array for dlx (Algorithm 2)
Input: List of item names
Output: STArray of Item a
-}
itemArray :: [a] -> ST s (STArray s Int (Item a))
itemArray xs@(y:ys) =
  -- additional node in front of the items
  let list = Item {prev = length xs, name = y, next = 1}: helper xs 1 in
    newListArray (0, length list-1) list where
      helper [x] i = [Item {prev = i-1, name = x, next = 0}]
      helper (x:xs) i =
        let elem = Item {prev = i-1, name = x, next = i+1} in
        elem:helper xs (i+1)

{-
Description: Constructs the node array for dlx (Algorithm 3)
Input: List of options, itemArray
Output: STArray of Nodes
-}
nodeArray ::  Eq a =>  [[a]] -> STArray s Int (Item a) -> ST s (STArray s Int Node)
nodeArray matrix itemsArr = do
  assocs <- getAssocs itemsArr
  let itemNodes = createItemNodes assocs
  let nodeList = itemNodes ++ createNodes matrix assocs
  nodes <- newListArray (0, length nodeList-1) nodeList
  nodeAssocs <- getAssocs nodes
  updateSpacerRefs nodes nodeAssocs
  updateNodeRefs nodes nodeAssocs
  modifyArray nodes 0 (\n -> n {itm = -1})
  b <- getNumElements nodes
  fixItemsNodes nodes
  return nodes where

    fixItemsNodes nodes = do
      assc <- getAssocs nodes
      let r = filter (\(i,n) -> itm n == 0) assc
      fix nodes r where
        fix nodes [] = return ()
        fix nodes ((i, x):xs) = do
          modifyArray nodes i (\n -> n {up = i, down = i})
          fix nodes xs

    --creating extra nodes for all the items
    createItemNodes :: [(Int, Item a)] -> [Node]
    createItemNodes = map (const Node {up = 0, itm = 0, down = 0})

    --creating nodes, but without the references 
    createNodes :: Eq a => [[a]] -> [(Int, Item a)]  -> [Node]
    createNodes [] _ = [Node {up = -1, itm = -1, down = -1}] --spacer 
    createNodes (xs:xss) assocs = Node {up = -1, itm = -1, down = -1}: createNode xs assocs ++ createNodes xss assocs where --spacer 
      createNode xs assocs = map (\x -> Node {up = 0, itm = findTop x assocs, down = 0}) xs where
        findTop elem [] = -1
        findTop elem ((i,x):xs)  = if elem == name x && i/=0 then i else  -- i /= 0 because the first array elem is a spacer with name of i=1
          findTop elem xs

    --creating refs for the spacer nodes 
    updateSpacerRefs :: STArray s Int Node -> [(Int, Node)] -> ST s ()
    updateSpacerRefs nodes assocs = do
      is <- findSpacer nodes (map fst assocs)
      update nodes is where
        findSpacer nodes is = filterSTArray nodes (\node -> itm node == -1) is

        update nodes [j] = return ()
        update nodes (i:j:is) = do
          modifyArray nodes i (\n -> n {down = j-1})
          modifyArray nodes j (\n -> n {up = i+1})
          update nodes (j:is)

    --creating refs for all other nodes (vll überarbeiten, kann man bei der init von node schon machen)
    updateNodeRefs :: STArray s Int Node -> [(Int, Node)] -> ST s (STArray s Int Node)
    updateNodeRefs nodes assocs = do
      nonTopIndeces <- filterSTArray nodes (\node -> itm node > 0) (map fst assocs)
      itemGroupsNoTop <- group nodes nonTopIndeces 1
      itemGroups <- addTop itemGroupsNoTop 1
      updateNodes nodes itemGroups

      where
        --groups all indices of nodes which have the same (none <0) itm in sublists  
        group nodes []  n = return []
        group nodes is n = do
          sublist <- filterSTArray nodes (\node -> itm node == n) is
          removedSublist <- filterSTArray nodes (\node -> itm node /= n) is
          (sublist:) <$> group nodes removedSublist (n+1)

        --adds the top elements to each list 
        addTop [] i = return []
        addTop (xs:xss) i = ((i:xs):) <$> addTop xss (i+1)

        --updates references 
        updateNodes nodes [] = return nodes
        updateNodes nodes (is:iss) = do
          update nodes is
          updateNodes nodes iss where
            update nodes [i] = do
              top <- getItm nodes i -- top node index 
              addTopLen nodes top
              -- makes it circular 
              modifyArray nodes i (\n -> n {down = top})
              modifyArray nodes top (\n -> n {up = i})
            update nodes (i:j:is) = do
              top <- getItm nodes i
              addTopLen nodes top
              modifyArray nodes i (\n -> n {down = j})
              modifyArray nodes j (\n -> n {up = i})
              update nodes (j:is)

{-
Description: Adds +1 to the amount of an item in the node array
Input: nodeArray, integer of item with amount increase
Output: ST s ()
-}
addTopLen :: STArray s Int Node -> Int -> ST s ()
addTopLen nodes top = do
  topNode <- readArray nodes top
  modifyArray nodes top (\n -> n {itm = itm topNode + 1})

{-
Description: Subtracts -1 to the amount of an item in the node array
Input: nodeArray, integer of item with amount decrease
Output: ST s ()
-}
subTopLen :: STArray s Int Node -> Int -> ST s ()
subTopLen nodes top = do
  topNode <- readArray nodes top
  modifyArray nodes top (\n -> n {itm = itm topNode - 1})

{-
Description: function like filter, but gives back indices instead of STArray elements
Input: STArray, filter function, all indices of array
Output: filtered indices
-}
filterSTArray :: (Ix i) => STArray s i a -> (a -> Bool) -> [i] -> ST s [i]
filterSTArray _ _ [] = return []
filterSTArray arr f (i:is) = do
  elem <- readArray arr i
  if f elem then (i:) <$> filterSTArray arr f is else
    filterSTArray arr f is
{-
Description: modifies Array (copied from docs)
Input: mutable array, index, modify function
Output: ST s ()
-}
-- kann funktion nicht importieren :(
modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray marr i f = do
  (l,u) <- getBounds marr
  n <- getNumElements marr
  let idx = safeIndex (l,u) n i
  x <- unsafeRead marr idx
  unsafeWrite marr idx (f x)

{-
Description: returns name of index 
Input: itemArray, index
Output: name 
-}
getName :: STArray s Int (Item a) -> Int -> ST s a
getName items i = do
  item <- readArray items i
  return (name item)

{-
Description: return prev elem of index
Input: itemArray, index
Output: prev elem
-}
getPrev :: STArray s Int (Item a) -> Int -> ST s Int
getPrev items i = do
  item <- readArray items i
  return (prev item)

{-
Description: returns next elem of index
Input: itemArray, index
Output: next elem
-}
getNext :: STArray s Int (Item a) -> Int -> ST s Int
getNext items i = do
  item <- readArray items i
  return (next item)

{-
Description: returns itm of index
Input: nodeArray, index
Output: itm
-}
getItm :: STArray s Int Node -> Int -> ST s Int
getItm nodes i = do
  node <- readArray nodes i
  return (itm node)

{-
Description: returns up elem of index
Input: nodeArray, index
Output: up elem
-}
getUp :: STArray s Int Node -> Int -> ST s Int
getUp nodes i = do
  node <- readArray nodes i
  return (up node)

{-
Description: returns down elem of index
Input: nodeArray, index
Output: down elem
-}
getDown :: STArray s Int Node -> Int -> ST s Int
getDown nodes i = do
  node <- readArray nodes i
  return (down node)

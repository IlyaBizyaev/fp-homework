module Block1.Task3
  ( SearchTree(..)
  , isEmpty
  , size
  , find
  , insert
  , fromList
  , delete
  )
where

-- | A type to represent a simple search tree.
-- Nodes store multiple equal elements as a pair of value and its count.
data SearchTree a = STLeaf
                  | STNode a Integer (SearchTree a) (SearchTree a)
                  deriving (Show, Eq)

-- | Check if a search tree is empty.
isEmpty :: SearchTree a -> Bool
isEmpty STLeaf = True
isEmpty _      = False

-- | Get search tree size.
size :: SearchTree a -> Integer
size STLeaf             = 0
size (STNode _ cnt l r) = cnt + size l + size r

-- | Check if the given element is present in the search tree.
find :: (Eq a) => SearchTree a -> a -> Bool
find STLeaf                 _ = False
find (STNode element _ l r) x = element == x || find l x || find r x

-- | Insert an element into the search tree.
insert :: (Eq a, Ord a) => SearchTree a -> a -> SearchTree a
insert STLeaf x = STNode x 1 STLeaf STLeaf
insert (STNode element cnt l r) x
  | element == x = STNode element (cnt + 1) l r
  | element < x  = STNode element cnt l (insert r x)
  | otherwise    = STNode element cnt (insert l x) r

-- | Build a search tree from a list.
fromList :: (Ord a) => [a] -> SearchTree a
fromList xs = foldl insert STLeaf xs

-- | Delete an element from a search tree.
-- Returns a pair of success flag and the resulting tree.
delete :: (Eq a, Ord a) => SearchTree a -> a -> (Bool, SearchTree a)
delete STLeaf _ = (False, STLeaf)
delete (STNode element cnt l r) x
  | element == x && cnt > 1
  = (True, STNode element (cnt - 1) l r)
  | element == x && isEmpty l
  = (True, r)
  | element == x && isEmpty r
  = (True, l)
  | element == x
  = let findMin STLeaf                = undefined
        findMin (STNode e c STLeaf _) = (e, c)
        findMin (STNode _ _ left   _) = findMin left
        (minNext, minNextCnt) = findMin r
        newR                  = snd $ delete r minNext
        newNode               = STNode minNext minNextCnt l newR
    in  (True, newNode)
  | element < x
  = let (success, newR) = delete r x in (success, STNode element cnt l newR)
  | otherwise
  = let (success, newL) = delete l x in (success, STNode element cnt newL r)

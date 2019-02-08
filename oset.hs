module OSet where

import Tree as U

-- %%%%%%%%%%%%%%%%%%%% DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -- 

type OSet a = Tree a

-- Data Constructors:

emptyset :: OSet a
emptyset = U.Empty

subset :: a -> OSet a
subset a = U.Node a 1 emptyset emptyset


-- %%%%%%%%%%%%%%%%%%% Inherrited Instances %%%%%%%%%%%%%% --
{-
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node a d l r) = Node (f a) d (fmap f l) (fmap f r)

instance Foldable Tree where
  foldl _ acc Empty = acc
  foldl f acc (Node a _ l r) = foldl f (f (foldl f acc l) a) r
  foldr _ acc Empty = acc
  foldr f acc (Node a _ l r) = foldr f (f a (foldr f acc r)) l
-}


-- %%%%%%%%%%%%% API Headers %%%%%%%%%%%%%%%%%%%%%%%%%%%%% --
-- Borrowed from tree.hs:

newSet :: (Ord a) => a -> OSet a                                                -- U.newTree
depth :: OSet a -> Int                                                          -- U.depth
isEmpty :: OSet a -> Bool

insert :: (Ord a) => a -> OSet a -> OSet a                                      -- U.addOnce
--insertWith :: (Ord a, Ord b)  => (a -> b) -> [a] -> OSet b -> OSet b          -- U.addOnceWith
insertAll :: (Ord a) => [a] -> OSet a -> OSet a                                 -- U.addAllOnce
insertAllWith :: (Ord b)  => (a -> b) -> [a] -> OSet b -> OSet b         -- U.addAllOnceWith

element :: (Ord a) => a -> OSet a -> Bool                                       -- U.contains

fromList :: (Ord a) => [a] -> OSet a                                            -- U.fromList
fromListWith :: (Ord b) => (a -> b) -> [a] -> OSet b                            -- U.fromListWith

toList :: OSet a -> [a]                                                         -- U.toList
union :: (Ord a) => OSet a -> OSet a -> OSet a                                  -- U.merge

-- Class Specific Functions:

--intersection :: (Ord a) => OSet a -> OSet a -> OSet a
--complement :: (Ord a) => OSet a -> OSet a -> OSet a
--`minus` :: (Ord a) => OSet a -> OSet a -> OSet a
--times :: (Ord a) => OSet a -> OSet a -> OSet a
--times :: (Ord a) => OSet a -> OSet a -> OSet a
--timesWith :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> OSet a -> OSet b -> OSet c
--isSubset :: OSet a -> OSet a -> Bool
--equals :: OSet a -> OSet a -> Bool
--directProd :: (Ord a, Ord b) => OSet a -> OSet b  -> OSet (a,b)


-- %%%%%%%%%%%%% API IMPLEMENTATION %%%%%%%%%%%%%%%%%%%%% --
-- Borrowed functions

newSet = U.newTree
depth = U.depth
isEmpty = U.isEmpty
insert = U.addOnce
--insertWith = U.addOnceWith
insertAll = U.addAllOnce
insertAllWith = U.addAllOnceWith
element = U.contains
fromList = U.fromListOnce
fromListWith = U.fromListOnceWith
toList = U.toList
union = U.mergeOnce

-- New Functions

intersection :: (Ord a) => OSet a -> OSet a -> OSet a
intersection s1 s2 = foldr (\x acc -> if element x s2 then insert x acc else acc) emptyset s1

complement :: (Ord a) => OSet a -> OSet a -> OSet a
--complement s1 s2 = foldr (\x acc -> if element x s1 then acc else insert x acc) emptyset s2
complement = foldr (\x t -> remove x t)


isSubset' :: (Ord a) => OSet a -> OSet a -> (Bool, OSet a)
isSubset' Empty s            = (True, s)
isSubset' _ Empty            = (False, emptyset)
isSubset' (Node a _ l r) s   = let (s',b) = removed a s
                               in if not b then (False, s)
                                  else let (b',s'') = isSubset' l s'
                                       in if not b' then (False, s'')
                                          else isSubset' r s''

isSubset :: (Ord a) => OSet a -> OSet a -> Bool
isSubset s1 s2 = let (b,_) = isSubset' s1 s2 in b

equals :: (Ord a) => OSet a -> OSet a -> Bool
equals s1 s2 = let (b,s2') = isSubset' s1 s2 in b && OSet.isEmpty s2'


--                               in b && let (b',t'') = isSubset' l t' in b'' && isSubset' 

--minus :: (Ord a) => OSet a -> OSet a -> OSet a
--(minus) = complement

-- (x,y) not (?) ordered
times :: (Ord a) => OSet a -> OSet a -> OSet (a,a)
times s s' = foldr (\x acc -> union acc $ foldr (\y acc' -> insert (x,y) acc') emptyset s) emptyset s'

timesWith :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> OSet a -> OSet b -> OSet c
timesWith f s s' = foldr (\x acc -> union acc $ foldr (\y acc' -> add (f x y) acc') emptyset s') emptyset s

--directProd :: (Ord a, Ord b) => OSet a -> OSet b  -> OSet (a,b)
--directProd l r = foldr (\x acc -> foldr (\y acc' -> insert (x,y) acc') acc r) emptyset l

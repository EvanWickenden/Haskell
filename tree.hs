{-# LANGUAGE GADTs              #-}

module Tree where


--import Control.RMonad

-- %%%%%%%%% TYPE %%%%%%%%%%%%%%%%%%%% -- 

data Tree a = Node a Int (Tree a) (Tree a) | Empty


{- %%%%%%%%%% API HEADERS %%%%%%%%%%%% -- 

newTree :: (Ord a) => a -> Tree a
depth :: Tree a -> Int
isEmpty :: Tree a -> Bool

add :: (Ord a) => a -> Tree a -> Tree a
addOnce :: (Ord a) => a -> Tree a -> Tree a
addAll :: (Ord a) => [a] -> Tree a -> Tree a

contains :: (Ord a) => a -> Tree a -> Bool
remove :: (Ord a) => a -> Tree a -> Tree a
removed :: (Ord a) => a -> Tree a -> (Tree a, Bool)
size :: Tree a -> Int

addAllWith :: (Ord a, Ord b)  => (a -> b) -> [a] -> Tree b -> Tree b
addAllOnce :: (Ord a) => [a] -> Tree a -> Tree a
addAllOnceWith :: (Ord a, Ord b)  => (a -> b) -> [a] -> Tree b -> Tree b
fromList :: (Ord a) => [a] -> Tree a
fromListWith :: (Ord a, Ord b) => (a -> b) -> [a] -> Tree b
fromListOnce :: (Ord a) => [a] -> Tree a
fromListOnceWith :: (Ord a, Ord b) => (a -> b) -> [a] -> Tree b
toList :: Tree a -> [a]
merge :: (Ord a) => Tree a -> Tree a -> Tree a
treeSort :: (Ord a) => [a] -> [a]

-}


-- %%%%%%%%%%%% INSTANCES %%%%%%%%%%%%% --

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node a d l r) = Node (f a) d (fmap f l) (fmap f r)

instance Foldable Tree where
  foldl _ acc Empty = acc
  foldl f acc (Node a _ l r) = foldl f (f (foldl f acc l) a) r
  foldr _ acc Empty = acc
  foldr f acc (Node a _ l r) = foldr f (f a (foldr f acc r)) l

--instance Applicative Tree where 
--  pure a = newTree a -- need type constraint on b
--  (<*>) (Node f _ fl fr) a = merge (fmap f a) $ merge (fl <*> a) (fr <*> a)


-- %%%%%%%%%%%%% API BODY %%%%%%%%%%%% --

newTree :: (Ord a) => a -> Tree a
newTree a = Node a 1 Empty Empty

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ d _ _) = d

left :: Tree a -> Tree a
left (Node _ _ l _) = l

right :: Tree a -> Tree a
right (Node _ _ _ r) = r

isEmpty :: Tree a -> Bool
isEmpty Empty   = True
isEmpty _       = False

add :: (Ord a) => a -> Tree a -> Tree a
add n Empty = newTree n
add n (Node a d l r) 
  | n < a       = let l' = add n l 
                  in balance $ Node a ((+) 1 $ max (depth l') (depth r)) l' r
  | otherwise   = let r' = add n r 
                  in balance $ Node a ((+) 1 $ max (depth l) (depth r')) l r'


contains :: (Ord a) => a -> Tree a -> Bool
contains n Empty = False 
contains n (Node a _ l r) 
  | n < a       = contains n l
  | n > a       = contains n r 
  | otherwise   = True -- trichotomy 


removed :: (Ord a) => a -> Tree a -> (Tree a, Bool)
removed n Empty = (Empty, False)
removed n t@(Node a d l r)
  | n < a             = let (l',b) = removed n l
                            t' =  if not b then t 
                                  else balance $ Node a (depth' l' r) l' r
                        in (t', b)

  | n > a             = let (r',b) = removed n r
                            t' =   if not b then t
                                  else balance $ Node a (depth' l r') l r'
                        in (t', b)

  | n == a && d == 1  = (Empty, True)

  | n == a && depth l > depth r
                      = let (Node al dl ll rl) = l 
                            r' = adjoinTree rl r
                        in (balance $ Node al (depth' ll r') ll r', True)

  | n == a && depth r >= depth l
                      = let (Node ar dr lr rr) = r
                            l' = adjoinTree lr l
                        in (balance $ Node ar (depth' l' rr) l' rr, True)


remove :: (Ord a) => a -> Tree a -> Tree a
remove n Empty = Empty
remove n t = let (t',_) = removed n t in t'

size :: Tree a -> Int
size Empty = 0
size (Node _ _ l r) = 1 + (size l) + (size r)

addOnce :: (Ord a) => a -> Tree a -> Tree a
addOnce n t = (\(a,_) -> a) $ addOnce' n t

addOnce' :: (Ord a) => a -> Tree a -> (Tree a, Bool)
addOnce' n Empty = (newTree n, False)
addOnce' n t@(Node a d l r) 
    | n < a       = let (l', b) = addOnce' n l
                        t' =  if b then t 
                              else balance $ Node a (depth' l' r) l' r
                    in  (t', b)
    | n > a       = let (r', b) = addOnce' n r
                        t' =  if b then t
                              else balance $ Node a (depth' l r') l r' 
                    in (t', b)
    | n == a      = (t, True)

addAll :: (Ord a) => [a] -> Tree a -> Tree a
addAll = flip $ foldr add

addAllWith :: (Ord b)  => (a -> b) -> [a] -> Tree b -> Tree b
addAllWith f = flip $ foldr (\x t -> add (f x) t) 

addAllOnce :: (Ord a) => [a] -> Tree a -> Tree a
addAllOnce = flip $ foldr addOnce

addAllOnceWith :: (Ord b)  => (a -> b) -> [a] -> Tree b -> Tree b
addAllOnceWith f = flip $ foldr (\x t -> addOnce (f x) t) 

fromList :: (Ord a) => [a] -> Tree a
fromList = flip addAll Empty

fromListWith :: (Ord b) => (a -> b) -> [a] -> Tree b
fromListWith f  = flip (addAllWith f) Empty

fromListOnce :: (Ord a) => [a] -> Tree a
fromListOnce = flip addAllOnce Empty

fromListOnceWith :: (Ord b) => (a -> b) -> [a] -> Tree b
fromListOnceWith f = flip (addAllOnceWith f) Empty

toList :: Tree a -> [a]
toList = foldr (\x l -> x:l) [] 

merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge = foldr add 

mergeOnce :: (Ord a) => Tree a -> Tree a -> Tree a
mergeOnce = foldr addOnce 

treeSort :: (Ord a) => [a] -> [a]
treeSort = toList . fromList 


-- %%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%% -- 


isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node _ _ l r) = ((depth l - depth r)^2 <= 1) && isBalanced l && isBalanced r

resize :: Tree a -> Tree a
resize (Node a d l r) = Node a (1 + (max (depth l) (depth r))) l r

_ll :: Tree a -> Tree a
_lr :: Tree a -> Tree a
_rl :: Tree a -> Tree a
_rr :: Tree a -> Tree a

_ll (Node a _ (Node la _ ll lr) r) = resize $ Node la 0 ll (resize $ Node a 0 lr r)
_rr (Node a _ l (Node ra _ rl rr)) = resize $ Node ra 0 (resize $ Node a 0 l rl) rr
_lr (Node a _ l r) = resize . _ll $ Node a 0 (_rr l) r
_rl (Node a _ l r) = resize . _rr $ Node a 0 l (_ll r)

balance :: Tree a -> Tree a
balance Empty = Empty
balance t@(Node a d l r) 
  | (depth l) - (depth r) >= 2  = if (depth $ left l) > (depth $ right l) then _ll t else _lr t
  | (depth r) - (depth l) >= 2  = if (depth $ right r) > (depth $ left r) then _rr t else _rl t
  | otherwise                   = t

depth' :: Tree a -> Tree a -> Int
depth' l r = 1 + max (depth l) (depth r)

adjoinTree :: (Ord a) => Tree a -> Tree a -> Tree a
adjoinTree Empty t = t
adjoinTree s Empty = s
adjoinTree s@(Node n _ _ _) (Node a _ l r)
  | n < a           = let l' = adjoinTree s l
                      in balance $ Node a (depth' l' r) l' r 
  | otherwise       = let r' = adjoinTree s r
                      in balance $ Node a (depth' l r') l r'



module UniqueTree where

data UniqueTree a = Node a Int (UniqueTree a) (UniqueTree a) | Empty

newTree :: a -> UniqueTree a
newTree a = Node a 1 Empty Empty

depth :: UniqueTree a -> Int
depth Empty = 0
depth (Node _ d _ _) = d

left :: UniqueTree a -> UniqueTree a
left (Node _ _ l _) = l

right :: UniqueTree a -> UniqueTree a
right (Node _ _ _ r) = r

--add :: (Ord a) => a -> UniqueTree a -> UniqueTree a
--add n Empty = newTree n
--add n (Node a d l r) 
--  | n < a       = let l' = add n l 
--                  in balance $ Node a ((+) 1 $ max (depth l') (depth r)) l' r
--  | otherwise   = let r' = add n r 
--                  in balance $ Node a ((+) 1 $ max (depth l) (depth r')) l r'

add' :: (Ord a) => a -> UniqueTree a -> (UniqueTree a, Bool)
add' n Empty = (newTree n, False)
add' n t@(Node a d l r) 
    | n < a       = let (l', b) = add' n l
                        t' =  if b then t 
                              else balance $ Node a ((+) 1 $ max (depth l') (depth r)) l' r
                    in  (t', b)
    | n > a       = let (r', b) = add' n r
                        t' =  if b then t
                              else balance $ Node a ((+) 1 $ max (depth l) (depth r')) l r' 
                    in (t', b)
    | n == a      = (t, True)

add :: (Ord a) => a -> UniqueTree a -> UniqueTree a
add n t = (\(a,_) -> a) $ add' n t

resize :: UniqueTree a -> UniqueTree a
resize (Node a d l r) = Node a (1 + (max (depth l) (depth r))) l r

_ll :: UniqueTree a -> UniqueTree a
_lr :: UniqueTree a -> UniqueTree a
_rl :: UniqueTree a -> UniqueTree a
_rr :: UniqueTree a -> UniqueTree a

_ll (Node a _ (Node la _ ll lr) r) = resize $ Node la 0 ll (resize $ Node a 0 lr r)
_rr (Node a _ l (Node ra _ rl rr)) = resize $ Node ra 0 (resize $ Node a 0 l rl) rr
_lr (Node a _ l r) = resize . _ll $ Node a 0 (_rr l) r
_rl (Node a _ l r) = resize . _rr $ Node a 0 l (_ll r)

balance :: UniqueTree a -> UniqueTree a
balance Empty = Empty
balance t@(Node a d l r) 
  | (depth l) - (depth r) >= 2  = if (depth $ left l) > (depth $ right l) then _ll t else _lr t
  | (depth r) - (depth l) >= 2  = if (depth $ right r) > (depth $ left r) then _rr t else _rl t
  | otherwise                   = t

instance Functor UniqueTree where
  fmap _ Empty = Empty
  fmap f (Node a d l r) = Node (f a) d (fmap f l) (fmap f r)

instance Foldable UniqueTree where
  foldl _ acc Empty = acc
  foldl f acc (Node a _ l r) = foldl f (f (foldl f acc l) a) r
  foldr _ acc Empty = acc
  foldr f acc (Node a _ l r) = foldr f (f a (foldr f acc r)) l

addAll :: (Ord a) => [a] -> UniqueTree a -> UniqueTree a
addAll = flip $ foldr (\x t -> add x t) 

addAllWith :: (Ord a, Ord b)  => (a -> b) -> [a] -> UniqueTree b -> UniqueTree b
addAllWith f = flip $ foldr (\x t -> add (f x) t) 

--addAllOnce :: (Ord a) => [a] -> UniqueTree a -> UniqueTree a
--addAllOnce = flip $ foldr (\x t -> addOnce x t)

--addAllOnceWith :: (Ord a, Ord b)  => (a -> b) -> [a] -> UniqueTree b -> UniqueTree b
--addAllOnceWith f = flip $ foldr (\x t -> addOnce (f x) t) 

fromList :: (Ord a) => [a] -> UniqueTree a
fromList = flip addAll Empty

fromListWith :: (Ord a, Ord b) => (a -> b) -> [a] -> UniqueTree b
fromListWith f  = flip (addAllWith f) Empty

--fromListOnce :: (Ord a) => [a] -> UniqueTree a
--fromListOnce = flip addAllOnce Empty
--
--fromListOnceWith :: (Ord a, Ord b) => (a -> b) -> [a] -> UniqueTree b
--fromListOnceWith f = flip (addAllOnceWith f) Empty

toList :: UniqueTree a -> [a]
toList = foldr (\x l -> x:l) [] 

merge :: (Ord a) => UniqueTree a -> UniqueTree a -> UniqueTree a
merge t t' = foldr (\x t -> add x t) t t'

--instance Applicative UniqueTree where
--  pure a = newTree a
--  (<*>) (Node f _ fl fr) n = merge (fmap f n) $ merge (fl <*> n) (fr <*> n)

treeSort :: (Ord a) => [a] -> [a]
treeSort = toList . fromList 



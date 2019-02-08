module DirectProduct where


-- %%%%%%%%%%%%%%%%%%%% DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -- 

--type OSet a = Tree a
type DirProd a = Oset a

-- Data Constructors:

emptyset :: DirProd a
emptyset = U.Empty

subset :: a -> DirProd a
subset a = U.Node a 1 emptyset emptyset

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tree where


data Depth :: * where
  Zero :: Depth
  Layer :: Depth -> Depth

data TwoThreeTree (d :: Depth) (a :: *) :: * where
  Empty :: TwoThreeTree Zero a
  TwoNode :: TwoThreeTree d a
          -> a
          -> TwoThreeTree d a
          -> TwoThreeTree (Layer d) a
  ThreeNode :: TwoThreeTree d a
            -> a
            -> TwoThreeTree d a
            -> a
            -> TwoThreeTree d a
            -> TwoThreeTree (Layer d) a

deriving instance Functor (TwoThreeTree d)
deriving instance Show a => Show (TwoThreeTree d a)


data Insert (d :: Depth) (a :: *) :: * where
--  Grew :: TwoThreeTree (Layer d) -> Insert d a -- Always gonna be a 2-node
  Grew :: TwoThreeTree d a -> a -> TwoThreeTree d a -> Insert d a
  Didn'tGrow :: TwoThreeTree d a -> Insert d a


insert' :: Ord a => a -> TwoThreeTree d a -> Insert d a
insert' x Empty = Grew Empty x Empty
-- insert' x (TwoNode Empty y Empty) =
--   Didn'tGrow (ThreeNode Empty (min x y) Empty (max x y) Empty)
-- insert' x (ThreeNode Empty y Empty z Empty)
--   | x < y     = Grew (TwoNode Empty x Empty) y (TwoNode Empty z Empty)
--   | x >= z    = Grew (TwoNode Empty y Empty) z (TwoNode Empty x Empty)
--   | otherwise = Grew (TwoNode Empty y Empty) x (TwoNode Empty z Empty)
insert' x (TwoNode l y r)
  | x < y =
    case insert' x l of
      Didn'tGrow l' ->
        Didn'tGrow (TwoNode l' y r)
      Grew ll z lr ->
        Didn'tGrow (ThreeNode ll z lr y r)
  | otherwise =
    case insert' x r of
      Didn'tGrow r' ->
        Didn'tGrow (TwoNode l y r')
      Grew rl z rr ->
        Didn'tGrow (ThreeNode l y rl z rr)
insert' x (ThreeNode l y m z r)
  | x < y =
    case insert' x l of
      Didn'tGrow l' -> Didn'tGrow (ThreeNode l' y m z r)
      Grew ll w lr -> Grew (TwoNode ll w lr) y (TwoNode m z r)
  | x >= z =
    case insert' x r of
      Didn'tGrow r' -> Didn'tGrow (ThreeNode l y m z r')
      Grew rl w rr -> Grew (TwoNode l y m) z (TwoNode rl w rr)
  | otherwise =
    case insert' x m of
      Didn'tGrow m' -> Didn'tGrow (ThreeNode l y m' z r)
      Grew ml w mr -> Grew (TwoNode l y ml) w (TwoNode mr z r)

data SomeTree (a :: *) :: * where
  SomeTree :: TwoThreeTree d a -> SomeTree a

deriving instance Show a => Show (SomeTree a)

insert :: Ord a => a -> SomeTree a -> SomeTree a
insert x (SomeTree t) =
  case insert' x t of
    Grew l m r -> SomeTree (TwoNode l m r)
    Didn'tGrow t' -> SomeTree t'

fromList :: (Ord a, Foldable t) => t a -> SomeTree a
fromList xs = foldr insert (SomeTree Empty) xs

toList :: SomeTree a -> [a]
toList (SomeTree t) =
  case t of
    Empty ->
      []
    TwoNode l x r ->
      toList (SomeTree l) ++ [x] ++ toList (SomeTree r)
    ThreeNode l x m y r ->
      toList (SomeTree l) ++ [x] ++ toList (SomeTree m) ++ [y] ++ toList (SomeTree r)

isSorted :: Ord a => SomeTree a -> Bool
isSorted t = isListSorted (toList t)
  where isListSorted [] = True
        isListSorted [x] = True
        isListSorted (x:y:xs) =
          x <= y && isListSorted (y:xs)

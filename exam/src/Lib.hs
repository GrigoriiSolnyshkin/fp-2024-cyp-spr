module Lib(rotate, Tree(..), InorderTree(..), PreorderTree(..), PostorderTree(..)) where

rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate _ [] = []
rotate r xs | r > 0 = case maybeLength r 0 xs of
    Nothing -> case splitAt r xs of (fp, sp) -> sp ++ fp
    Just l -> case splitAt (r `mod` l) xs of (fp, sp) -> sp ++ fp
rotate r xs = case length xs of l -> rotate (r `mod` l) xs

-- negative rotation with infinite lists won't work

maybeLength :: Int -> Int -> [a] -> Maybe Int
maybeLength 0 _ _ = Nothing
maybeLength _ accR [] = Just $ accR
maybeLength r accR (_:xs) = maybeLength (r - 1) (accR + 1) xs


data Tree a = Leaf | Node (Tree a) a (Tree a)  deriving (Eq, Show)

newtype InorderTree a = InOrder (Tree a) deriving (Eq, Show)
newtype PreorderTree a = PreOrder (Tree a) deriving (Eq, Show)
newtype PostorderTree a = PostOrder (Tree a) deriving (Eq, Show)

instance Foldable InorderTree where
  foldr f ini (InOrder Leaf) = ini
  foldr f ini (InOrder (Node left x right)) = foldr f (f x (foldr f ini $ InOrder right)) $ InOrder left

instance Foldable PreorderTree where
  foldr f ini (PreOrder Leaf) = ini
  foldr f ini (PreOrder (Node left x right)) = f x (foldr f (foldr f ini $ PreOrder right) $ PreOrder left)

instance Foldable PostorderTree where
  foldr f ini (PostOrder Leaf) = ini
  foldr f ini (PostOrder (Node left x right)) = foldr f (foldr f (f x ini) $ PostOrder right) $ PostOrder left


-- mapCPS :: (a -> b) -> [a] -> [b]
-- mapCPS f xs = do
--     x <- xs
--     return $ f x


mapCPS :: (a -> b) -> [a] -> ([b] -> x) -> x
mapCPS _ [] k = k []
mapCPS f (x:xs) k = mapCPS f xs $ \r -> k (f x: r) 

module Common
( applyIf
, replaceIth
) where

-- Apply a function to an argument if a condition is True, otherwise return
-- the original value
applyIf :: (a -> a) -> Bool -> a -> a
applyIf f cond x = if cond then f x else x

-- Replace the ith element of a list
replaceIth :: a -> Int -> [a] -> [a]
replaceIth element 0 (x:xs) = element:xs
replaceIth element index (x:xs) = x:(replaceIth element (index - 1) xs)

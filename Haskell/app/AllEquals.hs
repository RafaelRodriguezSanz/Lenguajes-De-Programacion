module AllEquals where

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs) = (x == y) && allEqual (y:xs)
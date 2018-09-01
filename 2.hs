{-# LANGUAGE Safe #-}

import Data.List
import Data.Monoid
import Data.Tuple
import System.IO

foldSum :: (Foldable t, Num a) => t a -> a
foldSum = getSum . foldMap Sum

minMaxRangeSum :: (Foldable t, Num a, Ord a) => [t a] -> a
minMaxRangeSum = foldSum . map minMaxRange
    where minMaxRange = (-) <$> maximum <*> minimum

getCombinations :: [a] -> [(a, a)]
getCombinations = concatMap (zip <$> (repeat . head) <*> tail) . (init . tails)

evenlyDivisible :: Integral a => a -> a -> Bool
evenlyDivisible x y
    | x == y    = True
    | otherwise = let (lesser, greater) = if x < y then (x, y) else (y, x)
                  in greater `rem` lesser == 0

evenlyDivisiblePairQuotients :: Integral a => [[a]] -> [a]
evenlyDivisiblePairQuotients = map (pairQuotient . swap)
                               . concatMap getEvenlyDivisiblePairs
                               . map (getUniqueCombinations)
    where pairQuotient = uncurry quot
          getEvenlyDivisiblePairs = filter (uncurry evenlyDivisible)
          getUniqueCombinations = getCombinations . sort . nub

main = do
    withFile "2.txt" ReadMode (\hdl -> do
        spreadsheet <- fmap readSpreadsheet $ hGetContents hdl
        putStrLnShow $ minMaxRangeSum spreadsheet
        let quotients = evenlyDivisiblePairQuotients spreadsheet
        let quotSum = foldSum quotients
        putStrLnShow quotSum)
    where readSpreadsheet = (map $ map (read::String->Int) . words) . lines
          putStrLnShow = putStrLn . show

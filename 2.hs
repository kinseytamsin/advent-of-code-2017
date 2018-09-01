{-# LANGUAGE Safe #-}

import Data.List
import Data.Monoid
import Data.Tuple
import System.IO

minMaxRange :: (Foldable t, Num a, Ord a) => t a -> a
minMaxRange = (-) <$> maximum <*> minimum

foldSum :: (Foldable t, Num a) => t a -> a
foldSum = getSum . foldMap Sum

minMaxRangeSum :: (Foldable t, Num a, Ord a) => [t a] -> a
minMaxRangeSum = foldSum . map minMaxRange

getCombinations :: [a] -> [(a, a)]
getCombinations = concatMap (zip <$> (repeat . head) <*> tail) . (init . tails)

evenlyDivisible :: Integral a => a -> a -> Bool
evenlyDivisible = curry $ (== 0) . uncurry rem

evenlyDivisiblePairs :: Integral a => [(a, a)] -> [(a, a)]
evenlyDivisiblePairs = filter (uncurry evenlyDivisible) . map swap

evenlyDivisiblePairQuotients :: Integral a => [[a]] -> [a]
evenlyDivisiblePairQuotients = map (uncurry quot)
                               . concatMap evenlyDivisiblePairs
                               . map (getCombinations . sort . nub)

readSpreadsheet :: String -> [[Int]]
readSpreadsheet = (map $ map (read::String->Int) . words) . lines

putStrLnShow :: Show a => a -> IO ()
putStrLnShow = putStrLn . show

main = do
    withFile "2.txt" ReadMode (\hdl -> do
        spreadsheet <- fmap readSpreadsheet $ hGetContents hdl
        putStrLnShow $ minMaxRangeSum spreadsheet
        let quotients = evenlyDivisiblePairQuotients spreadsheet
        let quotSum = foldSum quotients
        putStrLnShow quotSum)

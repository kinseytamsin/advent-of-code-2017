{-# LANGUAGE Safe #-}

import Data.Char
import Data.Monoid
import System.IO

neighborDigits :: Int -> [Int] -> [(Int, Int)]
neighborDigits n = take <$> length <*> (zip <$> cycle <*> (drop n . cycle))

matchingDigitsSum :: Int -> [Int] -> Int
matchingDigitsSum n = foldSum
                      . map fst
                      . filter (identicalPair)
                      . neighborDigits n
    where foldSum = getSum . foldMap Sum
          identicalPair = uncurry (==)

main = do
    withFile "1.txt" ReadMode (\hdl -> do
        inputDigits <- fmap digits $ hGetLine hdl
        let inputMatchingDigitsSum = flip matchingDigitsSum inputDigits
        let putSum = putStrLnShow . inputMatchingDigitsSum
        putSum 1
        putSum $ (length inputDigits) `quot` 2)
    where putStrLnShow = putStrLn . show
          digits = map digitToInt

{-# LANGUAGE Safe #-}

import Data.List
import Data.List.Unique
import System.IO

countPred :: (a -> Bool) -> [a] -> Int
countPred p = length . filter p

noAnagrams :: [String] -> Bool
noAnagrams = allUnique . map sort

main = do
    withFile "4.txt" ReadMode (\hdl -> do
        passphrases <- fmap readInput $ hGetContents hdl
        let putCount = let countPredPassphrases = flip countPred passphrases
                       in putStrLnShow . countPredPassphrases
        putCount allUnique
        putCount allUniqueAndNoAnagrams)
    where readInput              = map words . lines
          putStrLnShow           = putStrLn . show
          allUniqueAndNoAnagrams = (&&) <$> allUnique <*> noAnagrams

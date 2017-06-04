module Main where

--TODO: test if files exist
--TODO: error if selected line does not exist
--TODO: fix special characters

import Prelude hiding (filter)
import System.Environment (getArgs)
import Data.String (words, lines)
import Data.List (isSubsequenceOf, delete)

main :: IO()
main = getArgs >>= \flags -> validate flags

validate :: [String] -> IO()
validate [] = putStr "Usage: NGramPredict ‹number› ‹model›" 
    >> putStr "‹file› ‹line› ‹column›\n"
validate xs = if length xs /= 5
    then putStr "Wrong number of arguments!\n"
    else (getNGram file line column) >>= \nGram 
        -> (getList (length nGram + 1) model) >>= \nGramList
        -> write $ sort num (filter nGram nGramList)
        where 
            num = read $ xs !! 0
            model = xs !! 1
            file = xs !! 2
            line = read $ xs !! 3
            column = read $ xs !! 4
           
getNGram :: String -> Int -> Int -> IO [String]
getNGram xs y z = (readFile xs) >>= \str 
    -> return 
    $ findNGram [] $ selectColumn  z $ selectLines y (map words (lines str))
    where 
        selectLines :: Int -> [[String]] -> [[String]]
        selectLines i js
            | i == 1 = take 1 js
            | length js >= i  = take 2 (drop (i - 2) js)
            | otherwise  = []
        selectColumn :: Int -> [[String]] -> [String]
        selectColumn i js =
            let t = init $ last js
            in
                if ((length t) + (sum $ map length t) + 1) > i
                then selectColumn i ([head js] ++ [t])
                else concat js
        findNGram :: [String] -> [String] -> [String]
        findNGram is js 
            | length is == 4 = is
            | elem (last $ last js) ['.', ',', ';', '!', '?', '-'] = is
            | otherwise = findNGram ([last js] ++ is) (init js)

getList :: Int -> String -> IO [[String]]
getList x ys = (readFile ys) >>= \str 
    -> return $ take (amount x (lines str)) (beginning x (lines str))
    where
        beginning :: Int -> [String] -> [[String]]
        beginning i js = if head js /= "\\" ++ show i ++ "-grams:"
            then beginning i (tail js)
            else map words (tail js)
        amount :: Int -> [String] -> Int
        amount i js = if (take 7 $ head js) /= "ngram " ++ (show i)
            then amount i (tail js)
            else read $ drop 8 $ head js

filter :: [String] -> [[String]] -> [[String]]
filter xs ys = wildcard xs ys $ match [] xs ys
    where 
        match :: [[String]] -> [String] -> [[String]] -> [[String]]
        match xs _ [] = xs
        match is js ks = if not $ isSubsequenceOf js (init . init $ head ks)
            then match is js (tail ks)
            else match (is ++ [head ks]) (js) (tail ks)
        wildcard :: [String] -> [[String]] -> [[String]] -> [[String]]
        wildcard is js ks = if ks /= []
            then ks
            else concatMap (\str -> match [] str js) (split (length is) is) 
            where
                split :: Int -> [String] -> [[String]]
                split 1 _  = []
                split m ns = split (m - 1) ns ++ [delete (ns !! (m - 1)) ns]

sort :: Int -> [[String]] -> [[String]]
sort 0 _ = []
sort x ys = sort (x-1) ys ++ [max ys []]
    where 
        max :: [[String]] -> [String] -> []
        max [] ys = ys
        max xs ys = if head $ head xs > head ys
            then max (tail xs) (head xs)
            else max (tail xs) ys 

write :: [[String]] -> IO()
write xs =  

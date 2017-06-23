module Main where

import Prelude hiding (filter)
import System.Environment (getArgs)
import Data.String (words, lines)
import Data.List (isSubsequenceOf, delete, nubBy)

main :: IO()
main = getArgs >>= \flags -> validate flags

validate :: [String] -> IO()
validate [] = putStr "Usage: NGramPredict ‹number› ‹model›" 
    >> putStr " ‹file› ‹line› ‹column›\n"
validate xs = if length xs /= 5
    then putStr "Wrong number of arguments!\n"
    else getNGram file line column >>= \nGram -> loop xs nGram []
    where
        file = xs !! 2
        line = read $ xs !! 3
        column = read $ xs !! 4

getNGram :: String -> Int -> Int -> IO [String]
getNGram xs y z = readFile xs >>= \str 
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
                if (length t + sum (map length t) + 1) > i
                then selectColumn i (head js : [t])
                else concat js
        findNGram :: [String] -> [String] -> [String]
        findNGram is js 
            | length is == 4 = is
            | elem (last $ last js) ['.', ',', ';', '!', '?', '-', ')'] = is
            | otherwise = findNGram (last js : is) (init js)

loop :: [String] -> [String] -> [[String]] -> IO()
loop xs [] zs = if length zs >= num
    then write num zs
    else rest model (num - length zs) >>= \rList -> write num (zs ++ rList)
        where
            num = read $ head xs
            model = xs !! 1
            rest :: String -> Int -> IO [[String]]
            rest is j = getList 1 is >>= \nGList 
                -> return $ sort j (drop 3 nGList)
loop xs ys zs = if length zs >= num
    then loop [] [] zs
    else getList (length ys + 1) model >>= \nGList
        -> loop xs (tail ys) (nubBy tailEq (zs ++ sort num (filter ys nGList)))
        where
            num = read $ head xs
            model = xs !! 1
            tailEq :: [String] -> [String] -> Bool
            tailEq is js = last (init is) == last (init js)

getList :: Int -> String -> IO [[String]]
getList x ys = readFile ys >>= \str 
    -> return $ take (amount x (lines str)) (beginning x (lines str))
    where
        beginning :: Int -> [String] -> [[String]]
        beginning i js = 
            if head js /= "\\" ++ show i ++ "-grams:"
            then beginning i (tail js)
            else map words (tail js)
        amount :: Int -> [String] -> Int
        amount i js = 
            if take 7 (head js) /= "ngram " ++ show i
            then amount i (tail js)
            else read $ drop 8 $ head js

filter :: [String] -> [[String]] -> [[String]]
filter = match []
    where 
        match :: [[String]] -> [String] -> [[String]] -> [[String]]
        match is _ [] = is
        match is js ks = 
            if not $ isSubsequenceOf js (init . init $ head ks)
            then match is js (tail ks)
            else match (is ++ [head ks]) js (tail ks)

sort :: Int -> [[String]] -> [[String]]
sort 0 _ = []
sort _ [] = []
sort x ys = (\ele -> ele : sort (x-1) (delete ele ys))(maxP ys [])
    where 
        maxP :: [[String]] -> [String] -> [String]
        maxP [] js = js
        maxP is [] = maxP (tail is) (head is)
        maxP is js = 
            if head (head is) < head js
            then maxP (tail is) (head is)
            else maxP (tail is) js 

write :: Int -> [[String]] -> IO()
write _ [] = putStr "\n"
write 0 _ = putStr "\n"
write x ys = putStr (last . init $ head ys) >> putStr "\n" 
    >> write (x -1) (tail ys)

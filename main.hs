module Main where

import Data.Char (isPunctuation)
import qualified Data.Map.Strict as Map
import Data.String (words, lines)
import Data.List (sort, nubBy, isSubsequenceOf, isPrefixOf)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

main :: IO()
main = getArgs >>= validate

validate :: [String] -> IO()
validate xs = if length xs /= 5
    then hPutStr stderr "Usage: NGramPredict <number> <model> <file> "
        >> hPutStr stderr "<line> <column>\n"
    else readFile (xs !! 2) >>= \file ->
        readFile (xs !! 1) >>= \model ->
        write . take number . nubBy predEq . sort . predict (getModel model) [] $ getNGram file line column $ getMaxN (tail $ lines model) 0
        where
            number = read $ head xs
            line = read $ xs !! 3
            column = read $ xs !! 4
            predEq :: [String] -> [String] -> Bool
            predEq is js = (is !! 1) == (js !! 1)
            getMaxN :: [String] -> Int -> Int
            getMaxN is j = if head is == []
                then j
                else getMaxN (tail is) (read [(head is) !! 6])

getModel :: String -> Map.Map [String] [[String]]
getModel xs = traverse Map.empty $ lines xs
    where
        traverse :: Map.Map [String] [[String]] -> [String] -> Map.Map [String] [[String]]
        traverse is [] = is
        traverse is js 
            | head js == [] = traverse is (tail js)
            | isSubsequenceOf "\\" (head js)  = traverse is (tail js)
            | isPrefixOf "ngram" (head js)  = traverse is (tail js)
            | otherwise = traverse
                (Map.insertWith (++) torso ([(head line) : prediction : [bow]]) is)
                (tail js)
                where
                    torso = if (head $ last line) /= '-'
                        && (head $ last line) /= '0'
                        then init $ tail line
                        else init . init $ tail line
                    bow = if (head $ last line) /= '-'
                        && (head $ last line) /= '0'
                        then "-9999999.9"
                        else last line
                    prediction = if (head $ last line) /= '-'
                        && (head $ last line) /= '0'
                        then last line
                        else last $ init line
                    line = words $ head js

getNGram :: String -> Int -> Int -> Int -> [String]
getNGram ws x y z = 
    findNGram z [] $ selectColumn  y $ selectLines z x $ map words (lines ws)
    where 
        selectLines :: Int -> Int -> [[String]] -> [[String]]
        selectLines i j ks = if j > i 
            then take i $ drop (j - i) ks
            else take j ks
        selectColumn :: Int -> [[String]] -> [String]
        selectColumn i js =
            let t = init $ last js
            in
                if (length t + sum (map length t) + 1) > i
                then selectColumn i (init js ++ [t])
                else concat js
        findNGram :: Int -> [String] -> [String] -> [String]
        findNGram i js [] = js
        findNGram i js ks
            | length js == i = js
            | isPunctuation . last $ last ks = js
            | otherwise = findNGram i (last ks : js) (init ks)

predict :: Map.Map [String] [[String]] -> [String] -> [String] -> [[String]]
predict _ _ [] = []
predict xs [] zs = (Map.findWithDefault [] zs xs) ++ predict xs [head zs] (tail zs)
predict xs ys zs = predictions ++ predict xs (ys ++ [head zs]) (tail zs)
    where 
        predictions = correctP (select (last ys) (Map.findWithDefault [] (init ys) xs)) (Map.findWithDefault [] zs xs)
        select :: String -> [[String]] -> [String]
        select _ [] = []
        select [] _ = []
        select ms ns = if ms == ((head ns) !! 1)
            then head ns
            else select ms (tail ns)
        correctP :: [String] -> [[String]] -> [[String]]
        correctP [] ns = ns
        correctP _ [] = []
        correctP ms ns = ([show $ logBase 10.0 (10.0 ** (read $ last ms) * 10.0 ** (read . head $ head ns))]
            ++ (tail $ head ns)) : correctP ms (tail ns)

write :: [[String]] -> IO() 
write [] = putStr "\n"
write xs = putStr ((head xs) !! 1) >> putStr "\n" >> write (tail xs)

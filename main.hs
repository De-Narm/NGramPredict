{-|
    Module      : 
    Description : NGram Prediction of words
    Copyright   : 
    License     : BSD3
    Maintainer  : 
    Stability   : experimental
    Portability : POSIX

    Predicts the next word based on an input and a given model. 
-}

module Main (
    -- * NGramPredict
    main
    -- ** IO
    , validate
    , write
    -- ** Prediction
    , getNGram
    , getModel
    , predict
    )where

import Data.Char (isPunctuation)
import qualified Data.Map.Strict as Map
import Data.String (words, lines)
import Data.List (sort, nubBy, isSubsequenceOf, isPrefixOf)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

-- -----------------------------------------------------------------------------
-- NGramPredict main function

-- | Gets flags and validates them.
-- Required flags are ´NGramPredict \<number\> \<model\> 
-- \<file\> \<line\> \<column\>´
main :: IO()
main = getArgs >>= validate

-- -----------------------------------------------------------------------------
-- IO functions

-- | Controls whether or not enoughs arguments are given,
-- the base functions themselves test if the arguments
-- are of the right type. 
validate :: [String]    -- ^ runtime flags
    -> IO()             -- ^ final results
validate xs = if length xs /= 5
    then hPutStr stderr "Usage: NGramPredict <number> <model> <file> "
        >> hPutStr stderr "<line> <column>\n"
    else readFile (xs !! 2) >>= \file ->
        readFile (xs !! 1) >>= \model ->
        write . take number . nubBy predEq . sort . predict (getModel model) [] 
        $ getNGram file line column $ getMaxN (tail $ lines model) 0
        where
            number = read $ head xs
            line = read $ xs !! 3
            column = read $ xs !! 4
            predEq :: [String]  -- list [prob, prediction, bow]
                -> [String]     -- list [prob, prediction, bow]
                -> Bool         -- prediction == prediction?
            predEq is js = (is !! 1) == (js !! 1)
            getMaxN :: [String] -- arpa file, beginning with second line
                -> Int          -- current max length for n-grams
                -> Int          -- final max length
            getMaxN is j = if null $ head is
                then j
                else getMaxN (tail is) (read [head is !! 6])

-- | Writes predictions from a list.
write :: [[String]] -- ^ list of ["prob", "prediction", "bow"] 
    -> IO()         -- ^ final console output
write [] = putStr "\n"
write xs = putStr (head xs !! 1) >> putStr "\n" >> write (tail xs)

-- -----------------------------------------------------------------------------
-- Prediction functions

-- | Converts a .arpa file into a map of the form
-- [elements of (n-1)-gram] [["probability", "n-th element", "bow"]],
-- i.e. one key holds all possible predicions with their
-- probabilites as value.
getModel :: String                  -- ^ content or .arpa file
    -> Map.Map [String] [[String]]  -- ^ map of .arpa file
getModel xs = traverse Map.empty $ lines xs
    where
        traverse :: Map.Map [String] [[String]] -- current map of .arpa file
            -> [String]                         -- lines of .arpa file
            -> Map.Map [String] [[String]]      -- final map of .arpa file
        traverse is [] = is
        traverse is js 
            | null $ head js = traverse is (tail js)
            | isSubsequenceOf "\\" (head js)  = traverse is (tail js)
            | isPrefixOf "ngram" (head js)  = traverse is (tail js)
            | otherwise = traverse
                (Map.insertWith (++) torso [head line : pred : [bow]] is)
                (tail js)
                where
                    torso = if con
                        then init $ tail line
                        else init . init $ tail line
                    bow = if con
                        then []
                        else last line
                    pred = if con
                        then last line
                        else last $ init line
                    line = words $ head js
                    con = head (last line) /= '-'
                        && head (last line) /= '0'

-- | Builds a n-gram based on an input file and a position. Its length is
-- based on the maximum length of n-grams in the model.
-- When encountering punctuation, the n-gram stops regardless of its length.
getNGram :: String  -- ^ input file
    -> Int          -- ^ line
    -> Int          -- ^ column
    -> Int          -- ^ max length
    -> [String]     -- ^ final n-gram
getNGram ws x y z = 
    findNGram z [] $ selectColumn  y $ selectLines z x $ map words $ lines ws
    where 
        selectLines :: Int  -- max length of n-gram
            -> Int          -- line
            -> [[String]]   -- all lines, split into word
            -> [[String]]   -- selected line + (n-1) prior lines
        selectLines i j ks = if j > i 
            then take i $ drop (j - i) ks
            else take j ks
        selectColumn :: Int -- column
            -> [[String]]   -- selected lines, split into word
            -> [String]     -- selected word and all prior words
        selectColumn i js =
            let t = init $ last js
            in
                if (length t + sum (map length t) + 1) > i
                then selectColumn i (init js ++ [t])
                else concat js
        findNGram :: Int    -- max length
            -> [String]     -- current n-gram
            -> [String]     -- selected words
            -> [String]     -- final n-gram
        findNGram i js [] = js
        findNGram i js ks
            | length js == i = js
            | isPunctuation . last $ last ks = js
            | otherwise = findNGram i (last ks : js) (init ks)

-- | Gets all values using the current n-gram as key, then drops the first
-- element and searches again, correcting the probability with the bow
-- off all prior dropped elements.
predict :: Map.Map [String] [[String]]  -- ^ map of .arpa file
    -> [String]                         -- ^ current prefix
    -> [String]                         -- ^ current n-gram
    -> [[String]]                       -- ^ list [["prob", "pred", "bow"]]
predict _ _ [] = []
predict xs [] zs = Map.findWithDefault [] zs xs 
    ++ predict xs [head zs] (tail zs)
predict xs ys zs = predictions ++ predict xs (ys ++ [head zs]) (tail zs)
    where 
        predictions = correctP 
            (select (last ys) (Map.findWithDefault [] (init ys) xs)) 
            (Map.findWithDefault [] zs xs)
        select :: String    -- n-th element of ngram
            -> [[String]]   -- values from (n-1)-gram search
            -> [String]     -- ["prob", "pred", "bow"] of prefix n-gram
        select _ [] = []
        select [] _ = []
        select ms ns = if ms == (head ns !! 1)
            then head ns
            else select ms (tail ns)
        correctP :: [String]    -- prefix ["prob", "pred", "bow"]
            -> [[String]]       -- values of current n-gram
            -> [[String]]       -- current values with corrected probability
        correctP [] ns = ns
        correctP _ [] = []
        correctP ms ns = 
            (show (logBase 10.0 
            (10.0 ** read (last ms) * 10.0 ** (read . head $ head ns)))
            : tail (head ns)) : correctP ms (tail ns)

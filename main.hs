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
    , getNGram
    , getList
    , write
    -- ** Predict
    , loop
    , filter
    , sort
    )where

import Prelude hiding (filter)
import System.Environment (getArgs)
import Data.String (words, lines)
import Data.List (isSubsequenceOf, delete, nubBy)

-- -----------------------------------------------------------------------------
-- NGramPredict main function

-- | Gets flags and validates them.
-- Required flags are ´NGramPredict \<number\> \<model\> 
-- \<file\> \<line\> \<column\>´
main :: IO()
main = getArgs >>= \flags -> validate flags

-- -----------------------------------------------------------------------------
-- IO functions

-- | Controls whether or not enoughs arguments are given,
-- the base functions themselves test if the arguments
-- are of the right type
validate :: [String]    -- ^ runtime flags
    -> IO()             -- ^ return value
validate [] = putStr "Usage: NGramPredict ‹number› ‹model›" 
    >> putStr " ‹file› ‹line› ‹column›\n"
validate xs = if length xs /= 5
    then putStr "Wrong number of arguments!\n"
    else getNGram file line column >>= \nGram -> loop xs nGram []
    where
        file = xs !! 2
        line = read $ xs !! 3
        column = read $ xs !! 4

-- | A 4-gram is build based on the given line and column. 
-- Should the function encounter certain punctuation, 
-- a shorter n-gram is build.
-- No n-gram is build when the given word is followed up by punctuation.
getNGram :: String  -- ^ path to text file
    -> Int          -- ^ line
    -> Int          -- ^ column
    -> IO [String]  -- ^ return value
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

-- | Given a valid model, the function builds a list with all
-- n-grams of a certain length.
getList :: Int          -- ^ length of n-grams to search for
    -> String           -- ^ model file path
    -> IO [[String]]    -- ^ return value
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

-- | Writes a certain number of predictions from a list of n-grams.
write :: Int        -- ^ number of values to print
    -> [[String]]   -- ^ list of n-grams
    -> IO()         -- ^ return value
write _ [] = putStr "\n"
write 0 _ = putStr "\n"
write x ys = putStr (last . init $ head ys) >> putStr "\n" 
    >> write (x -1) (tail ys)

-- -----------------------------------------------------------------------------
-- Predict

-- | Loops through all possible sizes of n-grams and builds a list of
-- predictions. Results from longer n-grams appear first in the list.
-- If more predictions are used than actually found, the most common
-- 1-grams are searched.
loop :: [String]    -- ^ runtime flags
    -> [String]     -- ^ n-gram
    -> [[String]]   -- ^ list of predicted n-ngrams
    -> IO()         -- ^ return value
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

-- | Takes a list of n-grams and one (n-1)-gram. The resultig list
-- contains all n-grams the is (n-1)-gram is a prefix of.
filter :: [String]  -- ^ (n-1)-gram
    -> [[String]]   -- ^ list of n-ngrams
    -> [[String]]   -- ^ return value
filter = match []
    where 
        match :: [[String]] -> [String] -> [[String]] -> [[String]]
        match is _ [] = is
        match is js ks = 
            if not $ isSubsequenceOf js (init . init $ head ks)
            then match is js (tail ks)
            else match (is ++ [head ks]) js (tail ks)

-- | Performs a linear search until the sorted list is exactly the asked 
-- length. All remaining unsorted elements are dropped.
sort :: Int         -- ^ number of elements to sort
    -> [[String]]   -- ^ list of n-grams
    -> [[String]]   -- ^ return value
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

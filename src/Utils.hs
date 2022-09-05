module Utils
  ( median,
    average,
    safeMin,
    safeMax,
    padString,
    unique,
  )
where

import Data.List (genericLength, sort, sortBy)
import Data.Map.Lazy (fromListWith, toList)
import Data.Text qualified as T

padString :: String -> String
padString str = str <> T.unpack (T.replicate (12 - length str) " ")

average :: (Real a) => [a] -> Double
average [] = 0.0
average xs = realToFrac (sum xs) / genericLength xs

median :: (Ord a, Fractional a) => [a] -> Maybe a
median [] = Nothing
median x =
  if odd n
    then Just $ sort x !! (n `div` 2)
    else Just $ ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
  where
    n = length x

safeMin :: Ord a => [a] -> Maybe a
safeMin [] = Nothing
safeMin xs = Just $ minimum xs

safeMax :: Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax xs = Just $ maximum xs

unique :: (Ord a) => [a] -> [(a, Int)]
unique xs = sortBy (\a b -> compare (snd b) (snd a)) $ toList (fromListWith (+) [(x, 1) | x <- xs])

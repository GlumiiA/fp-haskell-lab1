module Problem22
  ( loadNames
  , totalScore1
  , totalScore2
  , totalScore3
  , totalScore4
  , totalScore5
  , nameValue
  ) where

import Data.List (sort)
import Data.Char (ord)

-- Общая функция чтения и сортировки имён
loadNames :: String -> IO [String]
loadNames path = do
  content <- readFile path
  let names = sort (read ("[" ++ content ++ "]") :: [String])
  return names

-- Вычисление "веса" имени
nameValue :: String -> Int
nameValue = sum . map (\c -> ord c - ord 'A' + 1)

-- 1. Обычная рекурсия
totalScore1 :: [String] -> Int
totalScore1 names = helper names 1
  where
    helper [] _ = 0
    helper (x:xs) i = nameValue x * i + helper xs (i + 1)

-- 2. Хвостовая рекурсия
totalScore2 :: [String] -> Int
totalScore2 names = go names 1 0
  where
    go [] _ acc = acc
    go (x:xs) i acc = go xs (i + 1) (acc + nameValue x * i)


-- 3. Модульный вариант с fold
totalScore3 :: [String] -> Int
totalScore3 names =
  let nameScores = zipWith (\i n -> (i + 1) * nameValue n) [0..] names
  in foldl (+) 0 nameScores


-- 4. Использование map
totalScore4 :: [String] -> Int
totalScore4 names =
  let nameValues = map nameValue names
      scores = zipWith (*) nameValues [1..]
  in sum scores


-- 5. Ленивые бесконечные списки
totalScore5 :: [String] -> Int
totalScore5 names = sum $ zipWith (*) (map nameValue names) [1..]

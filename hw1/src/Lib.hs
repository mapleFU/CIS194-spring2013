module Lib where

-- 1234 --> [1, 2, 3, 4]
toDigits :: Integer -> [Integer]
toDigits x = if x > 0 then (toDigits (x `div` 10)) ++ [( x - (x `div` 10) * 10)]  else []


rev :: [Integer] -> [Integer]
rev[] = []
rev(x:xs) = rev xs ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = rev (toDigits x)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x > 10 = sumDigits (toDigits x)
  | otherwise = x
sumDigits (x:xs) = x + sumDigits xs

someFunc :: IO ()

someFunc = do
  putStrLn $ show (toDigits 1234)
  putStrLn $ show (toDigitsRev 1234)
  putStrLn $ show $ sumDigits (toDigitsRev 1234)
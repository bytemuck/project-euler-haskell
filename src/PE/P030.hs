module PE.P030 (p030) where

import PE.Common (digits)

digitSum :: Int -> Int -> Int
digitSum e 0 = 0
digitSum e x = (x `mod` 10) ^ e + digitSum e (x `div` 10)

digitsSum :: Int -> [Int]
digitsSum e = [n | n <- [2 ^ e .. e * 9 ^ e], n == digitSum e n]

p030 :: IO ()
p030 = print . sum $ digitsSum 5
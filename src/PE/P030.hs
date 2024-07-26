module PE.P030 (p030) where

import Data.Function (on)
import PE.Common (digitSum, digits)

-- (10 ^ n) - 1 <= n * 9 ^ e
-- ((10 ^ n) - 1) / n <= 9 ^ e
-- log9 (((10 ^ n) - 1) / n) <= e

-- where n is the number of digits and e the exponent we solving for.

maxValue :: Int -> Int
maxValue e = (9 ^ e) * last (takeWhile (<= e) [round $ logBase 9 (fromIntegral ((10 ^ n) - 1) / fromIntegral n) | n <- [1 ..]])

digitsSum :: Int -> [Int]
digitsSum e = [n | n <- [2 ^ e .. maxValue e], n == digitSum e n]

p030 :: IO ()
p030 = print . sum $ digitsSum 5

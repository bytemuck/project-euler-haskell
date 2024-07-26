module PE.Common (digits, digitSum, divisors, primes, primeFactors, windowed, windowed', fib, fibInfinite) where

import Data.Char (digitToInt)
import Data.List (tails, transpose)

fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibInfinite :: (Integral a) => [a]
fibInfinite = 0 : next where next = 1 : zipWith (+) fibInfinite next

divisors :: (Integral a) => a -> [a]
divisors n = [x | x <- [1 .. (n - 1)], n `mod` x == 0]

digits :: (Show a, Integral a) => a -> [a]
digits n = [fromIntegral (digitToInt x) | x <- show n]

digitSum :: Int -> Int -> Int
digitSum e 0 = 0
digitSum e x = (x `mod` 10) ^ e + digitSum e (x `div` 10)

windowed :: (Real a) => Int -> [a] -> [[a]]
windowed n xs = transpose (take (fromIntegral n) (tails xs))

windowed' :: (Real a) => Int -> Int -> [a] -> [[a]]
windowed' n size xs = transpose (take (fromIntegral n) (every' size (tails xs)))
  where
    every' :: Int -> [a] -> [a]
    every' n xs = case drop (fromIntegral n - 1) xs of
        y : ys -> y : every' n ys
        [] -> []

primes :: (Integral a) => [a]
primes = 2 : filter (null . tail . primeFactors) [3, 5 ..]

primeFactors :: (Integral a) => a -> [a]
primeFactors n = factor n primes
  where
    factor :: (Integral a) => a -> [a] -> [a]
    factor n (p : ps)
        | p * p > n = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p : ps)
        | otherwise = factor n ps

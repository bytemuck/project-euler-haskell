module Main where

import qualified Data.Map as M
import Formatting (fprintLn, int, string, (%))
import Formatting.Clock (timeSpecs)
import Formatting.Formatters (text)
import PE.All
import System.Clock (Clock (Monotonic), getTime)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

solutions :: M.Map Int (IO ())
solutions =
    M.fromList
        [ (1, p001)
        , (2, p002)
        , (3, p003)
        , (4, p004)
        , (5, p005)
        , (6, p006)
        , (7, p007)
        , (8, p008)
        , (9, p009)
        , (10, p010)
        , (11, p011)
        , (12, p012)
        , (13, p013)
        , (14, p014)
        , (15, p015)
        , (16, p016)
        , (17, p017)
        , (18, p018)
        , (19, p019)
        , (20, p020)
        , (21, p021)
        , (22, p022)
        , (23, p023)
        , (24, p024)
        , (25, p025)
        , (26, p026)
        , (27, p027)
        , (28, p028)
        , (29, p029)
        , (30, p030)
        ]

solution :: Int -> Maybe (IO ())
solution number = M.lookup number solutions

main :: IO ()
main = do
    args <- getArgs
    case args of
        [number] -> case solution $ read number of
            Just result -> do
                start <- getTime Monotonic
                result
                end <- getTime Monotonic
                fprintLn (string % timeSpecs) "Time: " start end
            Nothing -> print "No solution yet."
        _ -> exitSuccess

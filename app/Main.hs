module Main where

import Text.Printf
import System.Environment

import Monitors.Battery (queryBattery)
import Monitors.Volume (queryVolume)
import Monitors.Net (queryNet)

usage :: IO String
usage = printf "%s battery | volume | net" <$> getProgName

main :: IO ()
main = do
    args <- getArgs
    output <- case args of
        ["battery"] -> queryBattery
        ["volume" ] -> queryVolume
        ["net"    ] -> queryNet
        _ -> usage
    putStrLn output

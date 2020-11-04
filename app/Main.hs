module Main where

import Text.Printf
import System.Environment

import Monitors.Battery (queryBattery)
import Monitors.Date (queryDate)
import Monitors.Net (queryNet)
import Monitors.Volume (queryVolume)

usage :: IO String
usage = printf "%s battery | volume | net" <$> getProgName

main :: IO ()
main = do
    args <- getArgs
    output <- case args of
        [ "bat"      ] -> queryBattery
        [ "vol"      ] -> queryVolume
        [ "net"      ] -> queryNet
        [ "date"     ] -> queryDate True
        [ "date-min" ] -> queryDate False
        _              -> usage
    putStrLn output

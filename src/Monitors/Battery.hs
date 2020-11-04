module Monitors.Battery (queryBattery) where

import Data.List
import Data.Maybe
import System.Process
import Text.Printf
import Text.Regex
import qualified Data.Map as M

import Monitors.Common

data BatteryData = BatteryData
    { battery :: String
    , state   :: String
    , percent :: Int
    , time    :: Maybe (Int,Int)
    }

dischargingIcons :: M.Map Int String
dischargingIcons = M.map buildIcon $ M.fromList
    [ ( 0   , 62861 )
    , ( 10  , 62841 )
    , ( 20  , 62842 )
    , ( 30  , 62843 )
    , ( 40  , 62844 )
    , ( 50  , 62845 )
    , ( 60  , 62846 )
    , ( 70  , 62847 )
    , ( 80  , 62848 )
    , ( 90  , 62849 )
    , ( 100 , 62840 )
    ]

chargingIcons :: M.Map Int String
chargingIcons = M.map buildIcon $ M.fromList
    [ ( 20  , 62853 )
    , ( 30  , 62854 )
    , ( 40  , 62855 )
    , ( 60  , 62856 )
    , ( 80  , 62857 )
    , ( 90  , 62858 )
    , ( 100 , 62852 )
    ]

alertIcon :: String
alertIcon = buildIcon 62850

unknownIcon :: String
unknownIcon = buildIcon 62864

fullIcon :: String
fullIcon = buildIcon 62851

getColor :: Int -> String
getColor charge = fromMaybe def (M.lookup getKey colors)
    where
        getKey
            | charge < 20                   = "red"
            | charge >= 20 && charge < 80   = "yellow"
            | charge >= 80 && charge <= 100 = "green"
            | otherwise                     = "active"
        def = "#ffffff"

iconSel :: Ord a => a -> M.Map a String -> String
iconSel a xs = fromMaybe def $ findKey a xs >>= lookupKey xs
    where
        findKey b ys = getNextAbove b (M.keys ys)
        lookupKey = flip M.lookup
        getNextAbove a = listToMaybe . dropWhile (< a) . sort
        def = unknownIcon

getIcon :: BatteryData -> String
getIcon BatteryData { state = "Charging", percent = charge } = iconSel charge chargingIcons
getIcon BatteryData { state = "Discharging", percent = charge } = iconSel charge dischargingIcons
getIcon BatteryData { state = "Full" } = fullIcon
getIcon BatteryData { state = "Unknown" } = unknownIcon
getIcon _ = unknownIcon

parseBatteryInfo :: String -> Maybe BatteryData
parseBatteryInfo = fmap fmtBattery . matchRegex regex
    where
        regex = mkRegex "^(.+): (.+), ([0-9]+)%(, ([0-9]+):([0-9]+):[0-9]+ .+)?"
        fmtBattery [b,s,p,_,h,m] =
            let
                t = if h /= "" && m /= ""
                    then Just (read h,read m)
                    else Nothing
            in BatteryData
                { battery = b
                , state   = s
                , percent = read p
                , time    = t
                }

getColorIcon :: BatteryData -> String
getColorIcon BatteryData { state = "Full" } = colorize (colors M.! "active") fullIcon
getColorIcon x@BatteryData { percent = charge } = colorize (getColor charge) (getIcon x)

getStatus :: BatteryData -> String
getStatus x@BatteryData { state = "Full" } = getColorIcon x

getStatus x@BatteryData { state = "Unknown", percent = charge }
    | charge >= 95 = getStatus x{ state="Full" }
    | otherwise =
        let
            fmt = "%s %d"
            colorIcon = getColorIcon x
        in printf fmt colorIcon charge

getStatus x@BatteryData { state = "Charging", percent = charge } =
    let
        fmt = "%s %d"
        colorIcon = getColorIcon x
    in printf fmt colorIcon charge

getStatus x@BatteryData { state = "Discharging", percent = charge, time = (Just (h,m)) } =
    let
        fmt = "%s %d (%d:%02d)"
        colorIcon = getColorIcon x
    in printf fmt colorIcon charge h m

getStatus _ = colorize (colors M.! "active") unknownIcon

parseAcpiStdout :: String -> [BatteryData]
parseAcpiStdout = mapMaybe parseBatteryInfo . lines

fmtData :: [BatteryData] -> String
fmtData = (separator ++ ) . intercalate separator . map getStatus

runAcpi :: IO String
runAcpi = readProcess "acpi" ["-b"] ""

queryBattery :: IO String
queryBattery = fmtData . parseAcpiStdout <$> runAcpi

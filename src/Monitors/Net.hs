module Monitors.Net (queryNet) where

import Data.List.Split
import Data.List
import Data.Maybe
import System.Process
import Text.Printf
import Text.Regex
import qualified Data.Map as M

import Monitors.Common

data DeviceType = Wifi | Ethernet | OtherDevice

data DeviceData = DeviceData
    { deviceName   :: String
    , deviceType   :: DeviceType
    , connected    :: Bool
    , signal       :: Maybe Int
    }

icons :: M.Map String String
icons = M.map buildIcon $ M.fromList
    [ ( "wifi"     , 64168 )
    , ( "wifi-off" , 64169 )
    , ( "ethernet" , 63231 )
    ]

offline :: String
offline = colorize (colors M.! "inactive") (icons M.! "wifi-off")

getConnectivity :: IO String
getConnectivity = head . lines <$> readProcess "nmcli" args ""
    where args = ["networking", "connectivity", "check"]

getDevStatus :: IO [DeviceData]
getDevStatus = readProcess "nmcli" args "" >>= mapM buildDeviceData . lines
    where
        args = ["--terse", "--fields", "device,type,state", "device", "status"]

buildDeviceData :: String -> IO DeviceData
buildDeviceData x =
    let
        [d,t,state] = splitOn ":" x
        deviceType  = readDeviceType t
        connected   = state == "connected"
    in do
        signal <- getWifiSignal deviceType d
        return DeviceData
                { deviceName   = d
                , deviceType   = deviceType
                , connected    = connected
                , signal       = signal
                }

readDeviceType :: String -> DeviceType
readDeviceType "wifi"     = Wifi
readDeviceType "ethernet" = Ethernet
readDeviceType _          = OtherDevice

getWifiSignal :: DeviceType -> String -> IO (Maybe Int)
getWifiSignal Wifi dev = parseStdout <$> readProcess "nmcli" args ""
    where
        args =
            [ "--terse"
            , "--fields", "in-use,signal"
            , "device", "wifi", "list"
            , "ifname", dev
            ]
        splt = splitOn ":"
        isActive = (=="*") . head . splt
        readVal = read . (!! 1) . splt
        parseStdout = fmap readVal . find isActive . lines
getWifiSignal _ _ = return Nothing

getActiveDevs :: IO [DeviceData]
getActiveDevs = filter activeDev <$> getDevStatus
    where
        activeDev :: DeviceData -> Bool
        activeDev DeviceData { deviceType = Wifi,     connected = True } = True
        activeDev DeviceData { deviceType = Ethernet, connected = True } = True
        activeDev _ = False

getSignalColor :: Maybe Int -> String
getSignalColor x = fromMaybe (colors M.! def) (M.lookup (getKey x) colors)
    where
        def = "active"
        getKey Nothing = def
        getKey (Just x)
            | x < 30              = "red"
            | x >= 30 && x < 60   = "yellow"
            | x >= 60 && x <= 100 = "green"
            | otherwise           = def


makeDevIcon :: DeviceData -> String

makeDevIcon DeviceData { deviceType = Ethernet } =
    colorize (colors M.! "active") (icons M.! "ethernet")

makeDevIcon DeviceData { deviceType = Wifi, signal = signal@(Just s) } =
    let
        colorIcon = colorize (getSignalColor signal) (icons M.! "wifi")
        txt = colorize (colors M.! "active") (show s)
    in printf "%s %s" colorIcon txt

makeDevIcon DeviceData { deviceType = Wifi, signal = Nothing } =
    colorize (getSignalColor Nothing) (icons M.! "wifi")

makeDevIcon _ = offline


getStatus :: [DeviceData] -> String
getStatus = intercalate separator . map makeDevIcon


queryNet :: IO String
queryNet = do
    connectivity <- getConnectivity
    --icon <- if connectivity `elem` ["none", "limited"]
    icon <- if connectivity == "none"
                then return offline
                else getStatus <$> getActiveDevs
    return $ separator ++ icon

module Monitors.Volume (queryVolume) where

import Data.List
import Data.Maybe
import System.Process
import Text.Printf
import Text.Regex
import qualified Data.Map as M

import Monitors.Common

mixer :: String
mixer = "Master"

icons :: M.Map String String
icons = M.map buildIcon $ M.fromList
    [ ( "high" , 64125 )
    , ( "low"  , 64126 )
    , ( "mid"  , 64127 )
    , ( "off"  , 64128 )
    , ( "mute" , 64605 )
    ]

data MixerData = MixerData
    { mute :: Bool
    , vol  :: Int
    }
    deriving Eq

getIcon :: MixerData -> String
getIcon x = fromMaybe (icons M.! def) $ M.lookup (iconKey x) icons
    where
        iconKey MixerData { mute = True } = "mute"
        iconKey MixerData { vol = vol }
            | vol >= 80             = "high"
            | vol >= 10 && vol < 80 = "mid"
            | vol < 10              = "low"
            | otherwise             = def
        def = "high"

getColor :: MixerData -> String
getColor x = fromMaybe (colors M.! def) $ M.lookup (colorKey x) colors
    where
        colorKey MixerData { mute = True } = "inactive"
        colorKey MixerData { vol = vol }
            | vol > 110               = "red"
            | vol > 100 && vol <= 110 = "yellow"
            | vol > 70  && vol <= 100 = "green"
            | vol > 0   && vol <=  70 = "blue"
            | vol == 0                = "inactive"
            | otherwise               = def
        def = "active"

getStatus :: MixerData -> String
getStatus x@MixerData { mute = mute, vol = vol } =
    let
        txtColor = if mute then colors M.! "inactive" else colors M.! "active"
        icon = colorize (getColor x) (getIcon x)
        txt = colorize txtColor $ show vol
    in printf "%s %s" icon txt

parseMixerInfo :: String -> Maybe MixerData
parseMixerInfo = fmap fmtMixer . matchRegex regex
    where
        regex = mkRegex ".+: Playback [0-9]+ \\[([0-9]+)%\\] (\\[.+dB\\] )?\\[(on|off)\\]"
        fmtMixer [volume,_,state] =
            let
                mute = case state of
                        "on"  -> False
                        "off" -> True
                        _     -> False
                vol = read volume
            in MixerData { mute = mute, vol = vol }

parseAmixerStdout :: String -> [MixerData]
parseAmixerStdout = nub . mapMaybe parseMixerInfo . lines

fmtData :: [MixerData] -> String
fmtData = (separator ++ ) . intercalate separator . map getStatus

runAmixer :: String -> IO String
runAmixer mixer = readProcess "amixer" ["get", mixer] ""


queryVolume :: IO String
queryVolume = fmtData . parseAmixerStdout <$> runAmixer mixer

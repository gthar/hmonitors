module Monitors.Date (queryDate) where

import Data.Time
import Text.Printf
import qualified Data.Map as M

import Monitors.Common


icons :: M.Map String String
icons = M.map buildIcon $ M.fromList
    [ ( "clock"    , 63055 )
    , ( "calendar" , 62957 )
    ]

fullFmtter :: Bool -> String -> String -> String
fullFmtter True icon x  = printf "%s %s" (icons M.! icon) x
fullFmtter _ _ x = x

fmtter :: FormatTime t => Bool -> String -> String -> t -> String
fmtter full icon fmt = (fullFmtter full icon) . (formatTime defaultTimeLocale fmt)

fmtTime :: FormatTime t => Bool -> t -> String
fmtTime full time =
    let
        date = fmtter full "calendar" "%m/%d" time
        hour = fmtter full "clock"    "%H:%M" time
    in printf "%s%s%s%s " separator date separator hour

queryDate :: Bool -> IO String
queryDate full = fmtTime full <$> getZonedTime

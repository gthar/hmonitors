module Monitors.Common
    ( colors
    , colorize
    , separator
    , buildIcon
    ) where

import qualified Data.Map as M
import Text.Printf
import Data.Char

colors :: M.Map String String
colors = M.fromList
    [ ( "active"   , "#ebdbb2" )
    , ( "inactive" , "#a89974" )
    , ( "red"      , "#fb4944" )
    , ( "yellow"   , "#fabd2f" )
    , ( "green"    , "#b8bb26" )
    , ( "blue"     , "#83a587" )
    ]

colorize :: String -> String -> String
colorize = printf "<fc=%s>%s</fc>"

separator :: String
separator = printf " %s " . colorize (colors M.! "inactive") $ "|"

buildIcon :: Int -> String
buildIcon = printf "<fn=1>%c</fn>" . chr

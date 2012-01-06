import XMonad
import XMonad.ManageHook
import XMonad.Operations
import XMonad.Hooks.DynamicLog   ( PP(..), dynamicLogWithPP, dzenColor, wrap, defaultPP )
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Ratio
import Graphics.X11

statusBarCmd= "dzen2 -bg '#1a1a1a' -fg '#777777' -h 16 -w 1920 -sa c -e '' -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -ta l"

main = do din <- spawnPipe statusBarCmd
          xmonad $ withUrgencyHook NoUrgencyHook defaultConfig

                    { borderWidth	= 2
                	, terminal	= "urxvt"
                    , workspaces         = ["1:main", "2:mail", "3:web"] ++ map show [4 .. 9 :: Int]
                    , normalBorderColor = "#cccccc"
                	, focusedBorderColor = "#cd8b00" 
                	, modMask = mod4Mask
                    , manageHook = manageDocks <+> composeOne[isFullscreen -?> doFullFloat] <+> manageHook defaultConfig <+> myManageHook
                    , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
                    , logHook = dynamicLogWithPP $ myPP din 
                    }
                     where
                     tiled   = Tall nmaster delta ratio

                     -- The default number of windows in the master pane
                     nmaster = 1

                     -- Default proportion of screen occupied by master pane
                     ratio   = 2/(1+(toRational(sqrt(5)::Double))) -- golden
                     
                     -- Percent of screen to increment by when resizing panes
                     delta   = 5%100

myPP h = defaultPP 
                 { ppCurrent = wrap "^fg(#ffffff)^bg(#0066ff)^p(2)^i(/home/rohit/.xmonad/dzen/marker.xbm)" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
                  , ppVisible = wrap "^bg(grey30)^fg(grey75)^p(2)" "^p(2)^fg()^bg()"
                  , ppHidden = wrap "^fg(#ffffff)^bg()^p(2)^i(/home/rohit/.xmonad/dzen/marker.xbm)" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
                  , ppUrgent = wrap "^fg(#ffffff)^bg(#f0164c)^p(2)^i(/home/rohit/.xmonad/dzen/marker.xbm)" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
                  , ppHiddenNoWindows = id . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
                  , ppSep     = "  ^fg(#ffffff)^r(2x2)^fg()  "
                  , ppWsSep           = " "
                  , ppLayout  = dzenColor "#cccccc" "" .
                                (\x -> case x of
                                         "Tall" -> "tall ^i(/home/rohit/.xmonad/dzen/layout-tall.xbm)"
                                         "Mirror Tall" -> "mirror ^i(/home/rohit/.xmonad/dzen/layout-mtall.xbm)"
                                         "Full" -> "full ^i(/home/rohit/.xmonad/dzen/layout-full.xbm)"
                                         "Grid" -> "grid"
                                         "Tabbed" -> "tabbed"
                                )
                  , ppTitle   = dzenColor "white" "" . wrap "< " " >" 
--                  , ppTitle   = dzenColor "white" ""
                  , ppOutput   = hPutStrLn h
                  }

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className   =? c                 --> doFloat | c <- myFloats]
    , [ title       =? t                 --> doFloat | t <- myOtherFloats]
    , [ resource    =? r                 --> doIgnore | r <- myIgnores]
    , [ className   =? "Chrome"     --> doF (W.shift "3:web") ]
    , [ className   =? "Skype"     --> doF (W.shift "4") ]
    ]
    where
        myIgnores       = ["panel", "stalonetray", "trayer"]
        myFloats        = ["feh", "GIMP", "gimp", "gimp-2.4", "Galculator", "VirtualBox", "VBoxSDL"]
        myOtherFloats   = ["alsamixer", "Bon Echo Preferences", "Mail/News Preferences", "Bon Echo - Restore Previous Session"] 



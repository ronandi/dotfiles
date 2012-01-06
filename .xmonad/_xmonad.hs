--
    -- ~/.xmonad/xmonad.hs
--

-- import the necessary libraries

import XMonad
import XMonad.ManageHook
import XMonad.Operations
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SinkAll
import XMonad.Hooks.DynamicLog   ( PP(..), dynamicLogWithPP, dzenColor, wrap, defaultPP )
import XMonad.Hooks.ManageDocks
import XMonad.Layout 
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders   ( noBorders, smartBorders )
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified Data.Map as M
import Data.Bits ((.|.))
import Data.Ratio
import Graphics.X11
import System.IO
 
statusBarCmd= "dzen2 -bg '#1a1a1a' -fg '#777777' -h 16 -w 550 -sa c -e '' -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -ta l"
 
main = do din <- spawnPipe statusBarCmd
          xmonad $ defaultConfig
 
                     { borderWidth        = 2
                     , normalBorderColor  = "#333333"
                     , focusedBorderColor = "#ff0099" 
                     , workspaces         = ["1:main", "2:mail", "3:web"] ++ map show [4 .. 9 :: Int]
                     , terminal           = "urxvt"
                     , modMask            = mod1Mask
                     , manageHook            = manageHook defaultConfig <+> myManageHook
                     , logHook            = dynamicLogWithPP $ myPP din
                               , layoutHook         = toggleLayouts (noBorders Full) $
                                            smartBorders $ tiled ||| Mirror tiled ||| Full ||| Grid ||| tabbed shrinkText 
                     , keys               = \c -> myKeys c `M.union` keys defaultConfig c
                     , mouseBindings      = \c -> myMouse c `M.union` mouseBindings defaultConfig c
                     }
                     where
                     tiled   = Tall nmaster delta ratio

                     -- The default number of windows in the master pane
                     nmaster = 1

                     -- Default proportion of screen occupied by master pane
                     ratio   = 2/(1+(toRational(sqrt(5)::Double))) -- golden
                     
                     -- Percent of screen to increment by when resizing panes
                     delta   = 5%100


-- application control 
--
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className   =? c                 --> doFloat | c <- myFloats]
    , [ title       =? t                 --> doFloat | t <- myOtherFloats]
    , [ resource    =? r                 --> doIgnore | r <- myIgnores]
    , [ className   =? "Firefox-bin"     --> doF (W.shift "3:web") ]
    , [ className   =? "Opera"           --> doF (W.shift "3:web") ]
    , [ className   =? "Thunderbird-bin" --> doF (W.shift "2:mail") ]
    ]
    where
        myIgnores       = ["panel", "stalonetray", "trayer"]
        myFloats        = ["feh", "GIMP", "gimp", "gimp-2.4", "Galculator", "VirtualBox", "VBoxSDL"]
        myOtherFloats   = ["alsamixer", "Bon Echo Preferences", "Mail/News Preferences", "Bon Echo - Restore Previous Session"] 

-- modify/add default key binds
--
myKeys (XConfig {modMask = modm}) = M.fromList $
           [
           -- custom dmenu
           ((modm, xK_p), spawn "exe=`dmenu_path | dmenu -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -nb '#000000' -nf '#FFFFFF' -sb '#0066ff'` && eval \"exec $exe\"") -- %! Launch dmenu
           -- sink all floating windows
           ,  ((modm .|. shiftMask, xK_t), sinkAll)
           -- swap focused with master, or master with next in line 
           , ((modm, xK_Return), dwmpromote)
           , ((modm, xK_KP_Enter), dwmpromote)
           -- rotate slave clients
           , ((modm .|. shiftMask, xK_Tab   ), rotSlavesUp)
           -- cycle through non-empty workspaces
           , ((modm .|. shiftMask, xK_Right), moveTo Next NonEmptyWS)
           , ((modm .|. shiftMask, xK_Left), moveTo Prev NonEmptyWS)
           -- switch to previous workspace
           , ((modm, xK_z), toggleWS)
           -- toggle to fullscreen.
           , ((modm, xK_x), sendMessage ToggleLayout)
           -- session management
           , ((modm .|. shiftMask .|. controlMask, xK_k), spawn "xkill")
           , ((modm .|. shiftMask .|. controlMask, xK_End), spawn "sudo shutdown -h now")
           , ((modm .|. shiftMask .|. controlMask, xK_Delete), spawn "sudo shutdown -r now")
           , ((mod4Mask, xK_l), spawn "xscreensaver-command --lock")
           -- application hotkeys
           , ((mod4Mask, xK_w), spawn "firefox")
           , ((mod4Mask, xK_t), spawn "thunderbird")
           , ((mod4Mask, xK_e), spawn "thunar")
           , ((mod4Mask, xK_v), spawn "urxvt -e alsamixer")
           , ((0, xK_Print), spawn "scrot %Y%m%d-dublin.png -t 280x175")
           , ((shiftMask, xK_Print), spawn "scrot %Y%m%d-dublin.png -d 3 -t 280x175")
           ]
 
-- modify/add default mouse binds
-- 
myMouse (XConfig {modMask = modm}) = M.fromList $
           [ ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
           , ((modm, button4), (\_ -> moveTo Next NonEmptyWS)) 
           , ((modm, button5), (\_ -> moveTo Prev NonEmptyWS))
           ]

-- dynamiclog pretty printer for dzen
--
myPP h = defaultPP 
                 { ppCurrent = wrap "^fg(#ffffff)^bg(#0066ff)^p(2)^i(/home/thayer/.xmonad/dzen/marker.xbm)" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId    -- Trim the '[Int]:' from workspace tags
                  , ppVisible = wrap "^bg(grey30)^fg(grey75)^p(2)" "^p(2)^fg()^bg()"
                  , ppHidden = wrap "^fg(#ffffff)^bg()^p(2)^i(/home/thayer/.xmonad/dzen/marker.xbm)" "^p(2)^fg()^bg()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
                  , ppHiddenNoWindows = id . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
                  , ppSep     = "  ^fg(#ffffff)^r(2x2)^fg()  "
                  , ppWsSep           = " "
                  , ppLayout  = dzenColor "#cccccc" "" .
                                (\x -> case x of
                                         "Tall" -> "tall ^i(/home/thayer/.xmonad/dzen/layout-tall.xbm)"
                                         "Mirror Tall" -> "mirror ^i(/home/thayer/.xmonad/dzen/layout-mtall.xbm)"
                                         "Full" -> "full ^i(/home/thayer/.xmonad/dzen/layout-full.xbm)"
                                         "Grid" -> "grid"
                                         "Tabbed" -> "tabbed"
                                )
--                  , ppTitle   = dzenColor "white" "" . wrap "< " " >" 
                  , ppTitle   = dzenColor "white" ""
                  , ppOutput   = hPutStrLn h
                  }

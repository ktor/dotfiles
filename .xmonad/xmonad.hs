import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.SpawnOn
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Accordion
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet(greedyView)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import qualified Data.Map as M

myKeys x = [
            ((mod1Mask .|. shiftMask, xK_4),      spawn "/usr/bin/scra")
           , ((mod1Mask .|. controlMask, xK_s),    spawn "systemctl suspend && xsecurelock")
           , ((mod1Mask .|. controlMask, xK_l),    spawn "xsecurelock")
           , ((0                , 0x1008ff11),     spawn "amixer -c 1 sset Master 4-")
           , ((0                , 0x1008ff13),     spawn "amixer -c 1 sset Master 4+")
           , ((0                , 0x1008ff03),     spawn "xbacklight -inc -10%")
           , ((0                , 0x1008ff02),     spawn "xbacklight -inc +10%")
--            , ((modMask x,               xK_i     ), withFocused (keysResizeWindow (-10,-10) (1,1)))
--            , ((modMask x,               xK_o     ), withFocused (keysResizeWindow (10,10) (1,1)))
           ]

newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

myStartupHook = do
  setWMName "LG3D"
  spawn "gnome-session --session gnome-flashback-xmonad"
  spawn "nm-applet"
  spawn "fdpowermon"

myManageHook = composeAll (
  [ manageHook gnomeConfig
  , resource  =? "stalonetray" --> doIgnore
  , className =? "Unity-2d-panel" --> doIgnore
  , className =? "Unity-2d-launcher" --> doIgnore
  , className =? "dota2" --> doIgnore
  -- , className =? "DyingLightGame" --> doFloat doesn't work either way.
  , className =? "Verdun.x86_64" --> doIgnore
  , className =? "PillarsOfEternity" --> doIgnore
  , className =? "hl2_linux" --> doIgnore
  , className =? "eu4" --> doIgnore
  , className =? "csgo_linux" --> doIgnore
  , className =? "RocketLeague" --> doIgnore
  , className =? "Gimp"      --> doFloat
  , className =? "Vncviewer" --> doFloat
  , className =? "Steam" --> doFloat
  , className =? "steam" --> doIgnore -- bigpicture-mode
  , className =? "MainThrd" --> doFloat
  , manageDocks
  ])

main = do
    xmonad $ gnomeConfig {
         manageHook = myManageHook
       , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
       , startupHook = myStartupHook
       , modMask = mod4Mask
       , keys = newKeys
       , terminal = "terminator"
       -- , terminal = "alacritty"
       , handleEventHook =
         mconcat [ docksEventHook
                 , handleEventHook defaultConfig ]
       }

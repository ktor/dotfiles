{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE PackageImports #-}

import "base" Data.List   (delete)
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
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet(greedyView, Stack(Stack, focus))
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import qualified Data.Map as M

myKeys x = [
            ((0, xK_Print), spawn "flameshot gui")
           , ((mod1Mask .|. controlMask, xK_s), spawn "xsecurelock & (sleep 1 && systemctl suspend)")
           , ((mod1Mask .|. controlMask, xK_l), spawn "xsecurelock")
           , ((0, 0x1008FF11), spawn "amixer sset Master 2%- && amixer -c 2 sset Master 2%-")
           , ((0, 0x1008FF13), spawn "amixer sset Master 2%+ && amixer -c 2 sset Master 2%+")
           , ((0, 0x1008FF12), spawn "amixer set Master toggle")
           , ((0, 0x1008ff03), spawn "xbacklight -inc -10%")
           , ((0, 0x1008ff02), spawn "xbacklight -inc +10%")
           ]

newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

myStartupHook = do
  setWMName "LG3D" -- make Java GUI applications work
  spawn "gnome-session --session gnome-flashback-xmonad"
  spawn "nm-applet"
--  spawn "fdpowermon"
  spawn "wallpaper"
  spawn "xmobar"

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
       , layoutHook = avoidStruts $ toggleLayouts (noBorders Full) $ smartBorders $ lessBorders MyAmbiguity $ layoutHook defaultConfig
       , startupHook = myStartupHook
       , modMask = mod4Mask
       , keys = newKeys
       , terminal = "terminator"
       -- , terminal = "alacritty"
       , handleEventHook =
         mconcat [ docksEventHook
                 , handleEventHook defaultConfig ]
       }

data MyAmbiguity = MyAmbiguity deriving (Read, Show)

instance SetsAmbiguous MyAmbiguity where
    hiddens ::
      MyAmbiguity ->
      WindowSet -> Rectangle ->
      Maybe (Stack Window) ->
      [(Window, Rectangle)] ->
      [Window]
    hiddens _ _ _ Nothing xs                    = fst <$> init xs
    hiddens _ _ _ (Just (Stack {XMonad.StackSet.focus = x})) xs = delete x (fst <$> xs)


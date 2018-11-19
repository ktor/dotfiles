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
import XMonad.Core (X ,withDisplay ,io)
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Types (Rectangle)
import qualified Data.Map as M

import "xmonad-contrib" XMonad.Layout.Spacing
    ( SmartSpacingWithEdge
    , smartSpacingWithEdge
    )
import "xmonad-contrib" XMonad.Layout.LayoutModifier (ModifiedLayout)
import "xmonad-contrib" XMonad.Layout.Decoration
    ( Decoration
    , DefaultShrinker
    )
import "xmonad-contrib" XMonad.Actions.MouseResize   (MouseResize)
import "xmonad-contrib" XMonad.Layout.SimpleFloat
    ( SimpleDecoration
    , SimpleFloat
    , simpleFloat
    )
import "xmonad-contrib" XMonad.Layout.WindowArranger (WindowArranger)
import "xmonad-contrib" XMonad.Hooks.EwmhDesktops    (fullscreenEventHook)

myKeys x = [
            -- ((0, xK_Print), spawn "flameshot gui")
            ((0, xK_Print), spawn "shutter -s")
           , ((controlMask, 0x60), spawn "copyq toggle") -- CTRL-`
           , ((mod4Mask .|. controlMask, xK_s), spawn "xsecurelock & (sleep 1 && systemctl suspend)")
           , ((mod4Mask .|. controlMask, xK_l), spawn "xsecurelock")
           , ((mod4Mask .|. controlMask, xK_v), spawn "keepass --auto-type")
           , ((0, 0x1008FF11), spawn "amixer sset Master 2%- && amixer -c 2 sset Master 2%-")
           , ((0, 0x1008FF13), spawn "amixer sset Master 2%+ && amixer -c 2 sset Master 2%+")
           , ((0, 0x1008FF12), spawn "amixer set Master toggle")
           , ((0, 0x1008ff03), spawn "xbacklight -inc -10%")
           , ((0, 0x1008ff02), spawn "xbacklight -inc +10%")
           ]

newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

xdisplays :: X [Rectangle]
xdisplays = withDisplay $ io . getScreenInfo

myStartupHook = do
  rects <- xdisplays
  spawnOnce "xcape" -- xcape to use CTRL as ESC when pressed alone
  spawnOnce "unclutter -jitter=20" -- hide cursor after X seconds idle
  spawnOnce "/home/ktor/bin/wallpaper" -- download bing picture of a day and set as wallpaper with feh
  -- spawnOnce "compton -b --config /home/ktor/.compton.conf" -- window shadows
  spawnOnce  "dunst -follow keyboard -force_xinerama" -- Dunst is a lightweight replacement for the notification-daemons provided by most desktop environments.  
  spawnOnce "xmobar"
  spawnOnce "stalonetray"
  setWMName "LG3D" -- make Java GUI applications work
  spawnOnce "gnome-session --session gnome-flashback-xmonad"
  spawnOnce "nm-applet"
  spawnOnce "copyq"
--  spawn "fdpowermon"
  spawnOnce "wallpaper"

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
  , className =? "copyq" --> doFloat
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
    xmonad $ defaultConfig {
         manageHook = myManageHook
       , layoutHook = smartBorders $ avoidStruts myLayoutHook
       , startupHook = myStartupHook
       , borderWidth = 1
       , modMask = mod4Mask
       , keys = newKeys
       , terminal = "terminator"
       , handleEventHook = fullscreenEventHook <> docksEventHook
       }

myLayoutHook ::
    ModifiedLayout
        (ConfigurableBorder MyAmbiguity)
        (ModifiedLayout
            SmartSpacingWithEdge
            (Choose
                (Choose Tall (Choose (Mirror Tall) Full))
                (ModifiedLayout
                    (Decoration SimpleDecoration DefaultShrinker)
                    (ModifiedLayout
                        MouseResize
                        (ModifiedLayout WindowArranger SimpleFloat)))))
        Window

myLayoutHook = lessBorders MyAmbiguity $ smartSpacingWithEdge 4 $ layoutHook defaultConfig ||| simpleFloat

data MyAmbiguity = MyAmbiguity deriving (Read, Show)

instance SetsAmbiguous MyAmbiguity where
    hiddens ::
      MyAmbiguity ->
      WindowSet -> Rectangle ->
      Maybe (Stack Window) ->
      [(Window, Rectangle)] ->
      [Window]
    hiddens _ _ _ Nothing xs = fst <$> init xs
    hiddens _ _ _ (Just (Stack {XMonad.StackSet.focus = x})) xs = delete x (fst <$> xs)


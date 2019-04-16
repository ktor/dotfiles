{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE PackageImports #-}

import "base" Data.List   (delete)
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Actions.WindowBringer (gotoMenu)
import XMonad.Config.Gnome
import XMonad.Core (X ,withDisplay ,io)
import XMonad.Util.EZConfig(removeKeys, additionalKeys)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce
import "xmonad-contrib" XMonad.Layout.SimpleFloat (simpleFloat)
import "xmonad-contrib" XMonad.Hooks.EwmhDesktops    (fullscreenEventHook)

import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Types (Rectangle)
import Graphics.X11.ExtraTypes.XF86 ( xF86XK_MonBrightnessUp
                                    , xF86XK_MonBrightnessDown
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioLowerVolume
                                    , xF86XK_Display
                                    , xF86XK_ScreenSaver
                                    )
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- MAIN                                                                       --
--------------------------------------------------------------------------------

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "terminator"

fconsoleName :: String
fconsoleName = "fconsole"

floatingConsole :: String
floatingConsole =  myTerminal
                ++ "-c " ++ fconsoleName

main :: IO ()
main = do
    xmonad $ defaultConfig {
         terminal = myTerminal
       , borderWidth = 1
       , modMask = myModMask
       , workspaces = myWorkspaces
       , handleEventHook = fullscreenEventHook <> docksEventHook
       , manageHook = myManageHook
       , layoutHook = smartBorders $ myLayoutHook
       , startupHook = myStartupHook
       }
       `removeKeys`            defaultWorkspaceKeyMap
       `removeKeys`            defaultScreenKeyMap
       `removeKeys`            defaultComboMap
       `additionalKeys`        myWorkspaceKeyMap
       `additionalKeys`        myScreenKeyMap
       `additionalKeys`        myComboMap
       `additionalKeys`        myShortcutKeyMap

--------------------------------------------------------------------------------
-- Operation Changes                                                          --
--------------------------------------------------------------------------------

defaultWorkspaceKeyMap :: [(KeyMask, KeySym)]
defaultWorkspaceKeyMap =
    [(m .|. mod1Mask, n) | n <- [xK_1 .. xK_9], m <- [0, shiftMask]]

myWorkspaceKeyMap :: [((KeyMask, KeySym), X ())]
myWorkspaceKeyMap =
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces myWorkspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

defaultScreenKeyMap :: [(KeyMask, KeySym)]
defaultScreenKeyMap =
    [(m .|. mod1Mask, n) | n <- [xK_w, xK_e], m <- [0, shiftMask]]

myScreenKeyMap :: [((KeyMask, KeySym), X ())]
myScreenKeyMap =
    [ ( (m .|. myModMask, key), do
        ws <- screenWorkspace sc
        whenJust ws (windows . f)
        warpToScreen sc 0.618 0.618 -- additional mouse operation
      )
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

defaultComboMap :: [(KeyMask, KeySym)]
defaultComboMap =
    [ (mod1Mask                 , xK_Return)
    , (mod1Mask .|. shiftMask   , xK_Return)
    , (mod1Mask                 , xK_p)
    ]

myComboMap :: [((KeyMask, KeySym), X ())]
myComboMap =
    [ ((myModMask               , xK_Return), spawn myTerminal)
    , ((myModMask .|. shiftMask , xK_Return), windows W.swapMaster)
    , ((myModMask               , xK_c), spawn floatingConsole)
    , ((myModMask               , xK_g), gotoMenu)
    ]

--------------------------------------------------------------------------------
-- Additional Shortcuts                                                       --
--------------------------------------------------------------------------------

myShortcutKeyMap :: [((KeyMask, KeySym), X ())]
myShortcutKeyMap =
           [ ((0, xK_Print), spawn "shutter -s")
           , ((controlMask, 0x60), spawn "copyq toggle") -- CTRL-`

           , ((mod4Mask .|. controlMask, xK_s), spawn "env XSECURELOCK_NO_COMPOSITE=1 xsecurelock & (sleep 1 && systemctl suspend)")
           , ((mod4Mask .|. controlMask, xK_l), spawn "env XSECURELOCK_NO_COMPOSITE=1 xsecurelock")
           , ((mod4Mask .|. controlMask, xK_v), spawn "keepass --auto-type")

           , ((0,         xF86XK_AudioLowerVolume)  , spawn "amixer sset Master 2%- && amixer -c 2 sset Master 2%-")
           , ((0,         xF86XK_AudioRaiseVolume)  , spawn "amixer sset Master 2%+ && amixer -c 2 sset Master 2%+")
           , ((0,         xF86XK_AudioMute)         , spawn "amixer set Master toggle")
           , ((0,         xF86XK_MonBrightnessDown) , spawn "xbacklight -inc -10%")
           , ((0,         xF86XK_MonBrightnessUp)   , spawn "xbacklight -inc +10%")
           , ((0,         xF86XK_Display)           , rescreenExt)
           , ((shiftMask, xF86XK_Display)           , rescreenMir)
           ]

--------------------------------------------------------------------------------
-- Multi-Screen                                                               --
--------------------------------------------------------------------------------

cmdXrandrExt :: String
cmdXrandrExt = "~/.xmonad/displaymgt.sh"

cmdXrandrMir :: String
cmdXrandrMir = "~/.xmonad/displaymgt.sh --mirror"

rescreenExt :: X ()
rescreenExt = do
    wsCurrent <- gets (W.currentTag . windowset)
    windows $ W.greedyView wsGrave
    windows $ W.greedyView wsCurrent
    spawn   $ cmdXrandrExt ++ ";" ++ cmdSetWallpaper

rescreenMir :: X ()
rescreenMir = spawn $ cmdXrandrMir ++ ";" ++ cmdSetWallpaper

--------------------------------------------------------------------------------
-- Workspaces                                                                 --
--------------------------------------------------------------------------------

wsidToName :: (Integral i, Show i) => i -> String
wsidToName i
    | i >= 0 && i <= 9  = "ws#" ++ show i
    | i == 10           = "ws#g"
    | otherwise         = "ws#e"

-- special workspaces for new screen handling
wsZero :: String
wsZero  = wsidToName 0

wsOne :: String
wsOne   = wsidToName 1

wsGrave :: String
wsGrave = wsidToName 10

myWorkspaces :: [String]
myWorkspaces = map wsidToName [0 .. 10]

myWorkspaceKeys :: [KeySym]
myWorkspaceKeys = [xK_0 .. xK_9] ++ [xK_grave]

--------------------------------------------------------------------------------
-- Wallpaper                                                                  --
--------------------------------------------------------------------------------

cmdSetWallpaper :: String
cmdSetWallpaper = "~/bin/wallpaper.sh"

xdisplays :: X [Rectangle]
xdisplays = withDisplay $ io . getScreenInfo

--------------------------------------------------------------------------------
-- Auto Startup                                                               --
--------------------------------------------------------------------------------

myStartupHook = do
  rects <- xdisplays
  spawnOnce "xmobar"
  spawnOnce "stalonetray"
  setWMName "LG3D" -- make Java GUI applications work
  spawnOnce "nm-applet"
  spawnOnce "copyq"
--  spawn "fdpowermon"
  spawnOnce "wallpaper"
  spawnOnce "xcape" -- xcape to use CTRL as ESC when pressed alone
  spawnOnce "unclutter --jitter=20" -- hide cursor after X seconds idle
  spawnOnce "/home/ktor/bin/wallpaper" -- download bing picture of a day and set as wallpaper with feh
  -- spawnOnce "compton -b --config /home/ktor/.compton.conf" -- window shadows
  spawnOnce  "dunst -follow keyboard -force_xinerama" -- Dunst is a lightweight replacement for the notification-daemons provided by most desktop environments.

--------------------------------------------------------------------------------
-- Special Window Management                                                  --
--------------------------------------------------------------------------------

myManageHook = composeAll (
  [ manageHook gnomeConfig
  , resource  =? "stalonetray" --> doIgnore
  -- , className =? "copyq" --> doShift (gets (W.currentTag . windowset)
  , className =? "copyq" --> doFloat
  , className =? floatingConsole --> doFloat
  , className =? "Gimp"      --> doFloat
  , className =? "Vncviewer" --> doFloat
  , className =? "MainThrd" --> doFloat
  , manageDocks
  ])

--------------------------------------------------------------------------------
-- Window Layout                                                              --
--------------------------------------------------------------------------------

myLayoutHook = lessBorders MyAmbiguity $ layoutHook defaultConfig ||| simpleFloat

data MyAmbiguity = MyAmbiguity deriving (Read, Show)

instance SetsAmbiguous MyAmbiguity where
    hiddens ::
      MyAmbiguity ->
      WindowSet -> Rectangle ->
      Maybe (W.Stack Window) ->
      [(Window, Rectangle)] ->
      [Window]
    hiddens _ _ _ Nothing xs = fst <$> init xs
    hiddens _ _ _ (Just (W.Stack {W.focus = x})) xs = delete x (fst <$> xs)


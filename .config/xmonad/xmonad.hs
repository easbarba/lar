-- Import
--
import Data.Monoid
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS)
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat
  , doFullFloat
  , isDialog
  , transience'
  )
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Dmenu
import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import qualified Data.Map as M
import qualified XMonad.StackSet as W

------------------------------------------------------------------------
-- Startup hook
--
myStartupHook = do
  spawnOnce "s-wm-autostart"
  setWMName "LG3D" -- Keep xmobar/dock visible after xmonad restart
  return () -- Integrate Java Swing/GUI apps into XMonad layouts;

------------------------------------------------------------------------
-- Workspaces
--
--  default number/name of workspaces
myWorkspaces = ["editor", "read", "term", "www", "media"]

-- Window rules:
myManageHook =
  composeAll
    [ className =? "Emacs" --> doShift "editor"
    , className =? "Code" --> doShift "editor"
    , className =? "Atril" --> doShift "read"
    , className =? "evince" --> doShift "read"
    , className =? "okular" --> doShift "read"
    , className =? "calibre" --> doShift "read"
    , className =? "libreoffice" --> doShift "read"
    , className =? "libreoffice-writer" --> doShift "read"
    , className =? "Alacritty" --> doShift "term"
    , className =? "st" --> doShift "term"
    , className =? "Chromium" --> doShift "www"
    , className =? "Firefox" --> doShift "www"
    , className =? "nyxt" --> doShift "www"
    , className =? "mpv" --> doShift "media"
    , className =? "vlc" --> doShift "media"
    ]

------------------------------------------------------------------------
-- Properties
--
myBorderWidth = 0 -- Width of the window border in pixels.

myModMask = mod4Mask -- "windows key" - mod4Mask.

altMask = mod1Mask -- Binding mod1 to altMask variable

myNormalBorderColor = "#dddddd" -- Border colors for unfocused windows

myFocusedBorderColor = "#f2f207" -- Border colors for focused windows

------------------------------------------------------------------------
-- Layouts:
--
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- preferred programs
myTerminal = "alacritty"
myBrowser = "chromium"
myEditor = "emacs"
myLocker = "slock"

------------------------------------------------------------------------
-- Key Bindings
--
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    -- system programs
  [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
  , ((modm, xK_e), spawn myEditor)
  , ((modm, xK_b), spawn myBrowser)
  , ((modm, xK_l), spawn myLocker)
  , ((0, xK_Print), spawn "s-wm-shot --full")
  , ((modm,               xK_x), spawn "dmenu_run")
  , ((modm .|. shiftMask, xK_w), spawn "s-wm-volume --up")
  , ((modm .|. shiftMask, xK_s), spawn "s-wm-volume --down")
  , ((modm .|. shiftMask, xK_e), spawn "s-wm-volume --toggle")
  , ((modm .|. shiftMask, xK_a), spawn "s-media-audio")
  , ((modm, xK_v), spawn "s-media-getvideo")
  , ((modm, xK_p), spawn "s-media-play")
  , ((modm, xK_space), spawn "mpc toggle")
  , ((modm .|. altMask, xK_w), spawn "s-wm-backlight --up")
  , ((modm .|. altMask, xK_s), spawn "s-wm-backlight --down")
  , ((modm, xK_Tab), toggleWS) -- cicla are de trabalho
  , ((modm, xK_a), prevWS) -- anterior are de trabalho
  , ((modm, xK_d), nextWS) -- proximo are de trabalho
  , ((modm .|. altMask, xK_x), shellPrompt def)
  , ((0, xK_Insert), pasteSelection) -- X-selection-paste buffer
    -- xmonad features
    --
    -- close focused window
  , ((modm .|. altMask, xK_c), kill)
     -- Rotate through the available layout algorithms
  , ((modm .|. altMask, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
  , ((modm .|. altMask, xK_n), refresh)
    -- Move focus to the next window
  , ((modm .|. altMask, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
  , ((modm .|. altMask, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  , ((modm .|. altMask, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((modm .|. altMask, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((modm .|. altMask, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
  , ((modm .|. altMask, xK_h), sendMessage Shrink)
    -- Expand the master area
  , ((modm .|. altMask, xK_l), sendMessage Expand)
    -- Push window back into tiling
  , ((modm .|. altMask, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ((modm .|. altMask, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
  , ((modm .|. altMask, xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
  , ((modm .|. altMask, xK_q), io exitSuccess)
    -- Restart xmonad
  , ((modm .|. altMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
  ] ++
    -- mod-[1..9], Switch to workspace N & mod-shift-[1..9], Move client to workspace N
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

------------------------------------------------------------------------
-- main
--
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad defaults

defaults =
  def
      -- essenciais coisas
    { terminal = myTerminal
    , borderWidth = myBorderWidth
    , modMask = myModMask
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

      -- chaves mapeamento
    , keys = myKeys

      -- gancho, esboco
    , layoutHook = myLayout
    , manageHook = myManageHook
    , handleEventHook = myEventHook
    , logHook = myLogHook
    , startupHook = myStartupHook
    }

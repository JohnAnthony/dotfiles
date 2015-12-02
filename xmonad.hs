import Data.Bits ((.|.))
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.PhysicalScreens
import qualified XMonad.Core as XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as S
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

modKey = mod4Mask
modAndShift = modKey .|. shiftMask

myWorkspaces = map show [1..9] ++ ["X"]

myManageHook = composeAll
  [ className =? "mplayer2"         --> doFloat
  , className =? "MPlayer"          --> doFloat
  , className =? "Spring"           --> doFloat
  , className =? "Wine"             --> doFloat
  , className =? "plugin-container" --> doFloat
  , className =? "Pillars of Eternity" --> doFloat
  ]

termExec str = termCmd ++ " -e " ++ str

{- Cmds for keys -}
termCmd        = "urxvtcd"
termSessionCmd = termExec "tmux"
termSessACmd   = termExec "tmux attach"
restartCmd     = "xmonad --recompile && xmonad --restart"
audioMixerCmd  = termExec "alsamixer"
browserCmd     = "firefox"
editorCmd      = termExec "vim"
altEditorCmd   = termExec "dtach -A /tmp/emacs.dtach emacs -nw"
emailCmd       = termExec "mutt"
ircCmd         = termExec "dtach -A /tmp/irssi.dtach irssi"
mediaPlayerCmd = termExec "ncmpcpp"
replCmd        = termExec "ghci"
rssCmd         = termExec "dtach -A /tmp/newsbeuter.dtach newsbeuter"
sysmonCmd      = termExec "htop"
volupCmd       = "amixer set Master 3%+"
voldnCmd       = "amixer set Master 3%-"
muteCmd        = "amixer set Master toggle"
audioStopCmd   = "mpc stop"
audioPrevCmd   = "mpc prev"
audioNextCmd   = "mpc next"
audioPlayCmd   = "mpc toggle"
screenshotCmd  = "scrot"
runCmd         = "dmenu_run -fn *-terminus-*-*-*-*-*-120-*-*-*-*-*-*"
    ++ " -nf '#ffffff' -nb '#101010' -sf '#ffffff' -sb '#666666'"

myLayoutHook = avoidStruts $ smartBorders Full ||| smartBorders tiled
 where tiled   = Tall nmaster delta ratio
       nmaster = 1         -- Number windows in master pane
       ratio   = 1/2       -- Proportion of screen used by master pane
       delta   = 3/100     -- Fraction of screen for resize step

mySendToScreen s = do
  sendToScreen s
  viewScreen s

myKeys conf = M.fromList $ switchKeys ++ moveKeys ++
  -- Window management bindings
  [ ((modAndShift,    xK_r       ), spawn restartCmd)
  , ((modKey,         xK_q       ), kill)
  , ((modKey,         xK_Tab     ), windows S.focusDown)
  , ((modAndShift,    xK_Tab     ), windows S.focusUp)
  , ((modKey,         xK_j       ), windows S.focusDown)
  , ((modKey,         xK_k       ), windows S.focusUp)
  , ((modAndShift,    xK_j       ), windows S.swapDown)
  , ((modAndShift,    xK_k       ), windows S.swapUp)
  , ((modKey,         xK_space   ), sendMessage NextLayout)
  , ((modKey,         xK_h       ), sendMessage Shrink)
  , ((modKey,         xK_l       ), sendMessage Expand)
  , ((modAndShift,    xK_q       ), io exitSuccess)
  , ((modKey,         xK_Right   ), viewScreen 1)
  , ((modKey,         xK_Left    ), viewScreen 0)
  , ((modAndShift,    xK_Right   ), mySendToScreen 1)
  , ((modAndShift,    xK_Left    ), mySendToScreen 0)
  , ((modKey,         xK_u       ), viewScreen 1)
  , ((modKey,         xK_y       ), viewScreen 0)
  , ((modAndShift,    xK_u       ), mySendToScreen 1)
  , ((modAndShift,    xK_y       ), mySendToScreen 0)
  , ((modAndShift,    xK_space   ), withFocused $ windows . S.sink)
  -- Launching
  , ((modKey,         xK_r               ), spawn runCmd)
  , ((modKey,         xK_z               ), spawn sysmonCmd)
  , ((modKey,         xK_n               ), spawn browserCmd) 
  , ((modKey,         xK_i               ), spawn ircCmd)
  , ((modKey,         xK_e               ), spawn editorCmd)
  , ((modKeyShift,    xK_e               ), spawn altEditorCmd)
  , ((modKey,         xK_t               ), spawn termSessionCmd)
  , ((modAndShift,    xK_t               ), spawn termSessACmd)
  , ((modKey,         xK_Return          ), spawn termCmd)
  , ((modKey,         xK_Print           ), spawn screenshotCmd)
  , ((modKey,         xK_a               ), spawn audioMixerCmd)
  , ((0,              xF86XK_AudioLowerVolume ), spawn voldnCmd)
  , ((0,              xF86XK_AudioRaiseVolume ), spawn volupCmd)
  , ((0,              xF86XK_AudioMute   ), spawn muteCmd)
  , ((0,              xF86XK_Tools       ), spawn mediaPlayerCmd)
  , ((0,              xF86XK_AudioStop   ), spawn audioStopCmd)
  , ((0,              xF86XK_AudioPrev   ), spawn audioPrevCmd)
  , ((0,              xF86XK_AudioNext   ), spawn audioNextCmd)
  , ((0,              xF86XK_AudioPlay   ), spawn audioPlayCmd)
  , ((modKey,         xK_m               ), spawn emailCmd)
  , ((modKey,         xK_b               ), spawn rssCmd)
  , ((0,              xF86XK_Calculator  ), spawn replCmd)
  ]
 where workspaceKeys = [xK_1 .. xK_9] ++ [xK_0]
       keyPairs = zip myWorkspaces workspaceKeys
       switchKeys = map switchf keyPairs
       moveKeys   = map movef keyPairs
       switchf (a, b) = ((modKey, b), windows $ onCurrentScreen S.greedyView a)
       movef (a, b) = ((modAndShift, b), windows $ onCurrentScreen S.shift a)

myPP handle = xmobarPP
  { ppOutput = hPutStrLn handle
  , ppTitle = xmobarColor "red" "" . shorten 100
  , ppSort = getSortByTag
  }

myLogHook procs = mapM_ log $ zip [0..] procs
 where log (scr, handle) = (dynamicLogWithPP . marshallPP scr . myPP) handle

main = do
  xmprocs <- mapM (spawnPipe . (\n-> "xmobar -x" ++ show n)) [0..1]
  xmonad $ defaultConfig
    { normalBorderColor = "#444444"
    , focusedBorderColor = "red"
    , terminal = termCmd
    , layoutHook = myLayoutHook
    , manageHook = manageDocks <+> myManageHook
    --, handleEventHook = 
    , workspaces = withScreens 2 myWorkspaces
    , modMask = modKey
    , keys = myKeys
    --, mouseBindings =
    , borderWidth = 1
    , logHook = myLogHook xmprocs
    , startupHook = return ()
    , focusFollowsMouse = True
    , clickJustFocuses = True
    }

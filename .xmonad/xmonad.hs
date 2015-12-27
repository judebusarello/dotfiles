import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive(fadeInactiveLogHook)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

--myLogHook :: X ()
--myLogHook = fadeInactiveLogHook fadeAmount
    --where fadeAmount = 0.8

main = do
    xmproc <- spawnPipe "/bin/xmobar /home/jude/.xmobarrc"
	-- mapM_ spawn ["xcompmgr", "sh ~/.fehbg", "xrdb ~/.Xresources"]
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = logHooks xmproc
		, borderWidth = 0
		, focusFollowsMouse = False
		}
	    `additionalKeys`
        [ ((0, 0x1008ff12), spawn "amixer -q set Master toggle")
        , ((0, 0x1008ff11), spawn "amixer -q set Master 1- unmute")
        , ((0, 0x1008ff13), spawn "amixer -q set Master 1+ unmute")
        , ((0, 0x1008ff02), spawn "xbacklight -inc 10")
        , ((0, 0x1008ff03), spawn "xbacklight -dec 10")
        ]
--addedKeys :: [((ButtonMask, KeySym), X ())]
--addedKeys = [ ((mod4Mask,xK_F9), spawn "amixer set Master 1- unmute")]

logHooks :: Handle -> X ()
logHooks xmobar =
	composeAll
	[ fadeInactiveLogHook 0.30
	, dynamicLogWithPP xmobarPP
		{ ppTitle = xmobarColor "lightblue" "" . shorten 50
		, ppCurrent = xmobarColor "#bfb556" "" . wrap "[" "]"
		, ppHidden = xmobarColor "#4c8ea1" ""
		, ppOrder = \(ws:l:t:_) -> [ws]
		, ppOutput = hPutStrLn xmobar
		}
	]
        -- , modMask = mod4Mask     -- Rebind Mod to the Windows key
        -- } `additionalKeys`
        -- [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        -- , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        -- , ((0, xK_Print), spawn "scrot")
        -- ]

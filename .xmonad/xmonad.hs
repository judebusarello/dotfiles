import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO


main = do
    xmproc <- spawnPipe "/bin/xmobar /home/jude/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppTitle = xmobarColor "lightblue" "" . shorten 50
                        , ppCurrent = xmobarColor "#bfb556" "" . wrap "[" "]"
                        , ppHidden = xmobarColor "#4c8ea1" "" 
                        , ppOrder = \(ws:l:t:_) -> [ws]
                        , ppOutput = hPutStrLn xmproc
                        }
	}
        -- , modMask = mod4Mask     -- Rebind Mod to the Windows key
        -- } `additionalKeys`
        -- [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        -- , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        -- , ((0, xK_Print), spawn "scrot")
        -- ]

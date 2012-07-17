{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import qualified DBus.Client.Simple as D
import qualified Codec.Binary.UTF8.String as UTF8


myLayout = onWorkspace "4:messaging" messagingLayout $ tiled ||| Mirror tiled ||| Full  
         where tiled = spacing 5 $ Tall nmaster delta ratio  
               nmaster = 1  
               ratio = 3/5  
               delta = 5/100 
	       gridLayout = spacing 5 $ Grid
	       messagingLayout = withIM (18/100) (Role "buddy_list") gridLayout

myWorkspaces = ["1:main", "2:programming" ,"3:web" , "4:messaging", "5"]

myManageHook = composeAll 
       [ className =? "Xfce4-appfinder" --> doCenterFloat
       , className =? "Xfrun4"          --> doCenterFloat
       , className =? "Pidgin"          --> doShift "4:messaging"
       , className =? "Xfce4-notifyd"	--> doIgnore
       ]

myAdditionalKeys = [ ((mod4Mask, xK_f), spawn "firefox") 
                   , ((mod4Mask, xK_t), spawn "Terminal")
		   , ((mod4Mask, xK_m), spawn "Terminal -e mutt")
                   ]

main = do 
       spawn "xcompmgr"
       dbus <- D.connectSession
       getWellKnownName dbus
       xmonad $ xfceConfig
              { modMask = mod4Mask
	      , terminal = "Terminal"
	      , borderWidth = 1
	      , normalBorderColor = "#abc123"  
              , focusedBorderColor = "#456def"
	      , layoutHook = avoidStruts myLayout
	      , logHook = dynamicLogWithPP (prettyPrinter dbus)
	      , workspaces = myWorkspaces
	      , manageHook = myManageHook <+> manageHook xfceConfig
	      } `additionalKeys` myAdditionalKeys

-- Settings for xmonad-log-applet

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const "" --pangoColor "cyan" . wrap "{" "}" . pangoSanitize
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
		[D.AllowReplacement, D.ReplaceExisting, D.DoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = D.emit dbus
			     "/org/xmonad/Log"
			     "org.xmonad.Log"
			     "Update"
			     [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

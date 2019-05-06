{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where


import qualified GI.Gtk as Gtk
import Data.GI.Base
import GI.Gtk.Objects.Widget
import GI.Gtk.Objects.CssProvider
import GI.Gtk.Objects.StyleContext
import GI.Gtk.Constants
import           System.IO
import qualified System.Process as SP
import           Data.List
import           Data.List.Split
import           Text.Regex.PCRE
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as DA
import           Text.XML.HXT.Core
import           Text.XML.HXT.Arrow.Pickle
import           Data.Map
import           Data.Maybe
import qualified Data.Text as T
import           System.Directory

type ScreenRes = String
type WorkSpace = String
type WinID = String
type WinTitle = T.Text


data WindowPlacement = WindowPlacement {
  nick :: String,
  geometry :: String
  }
instance XmlPickler WindowPlacement where
  xpickle = xpWindow
xpWindow :: PU WindowPlacement
xpWindow =
  xpElem "window" $
  xpWrap ( \ ((nic, geo)) -> WindowPlacement nic geo
         , \t -> (nick t, geometry t)
         ) $
  xpPair (xpAttr "nick" xpText) (xpAttr "geometry" xpText)

getGeomList :: ScreenRes -> String -> IO [Maybe WindowPlacement]
getGeomList r homeDir  = runX $ readDocument [withRemoveWS yes] (homeDir ++ "/.wmjump/wint.xml")  >>>
  getChildren >>>
  getChildren >>>
  hasAttrValue "resolution" (\x -> x == r) >>>
  getChildren >>^
  unpickleDoc xpWindow

winsToMap :: [Maybe WindowPlacement] -> Map String String
winsToMap ws = fromList [ (nick (fromJust w), geometry (fromJust w)) | w <- ws , isJust w ]

getCurrentWSAndRes :: IO (WorkSpace, ScreenRes)
getCurrentWSAndRes = do
  (Just hin, Just hout, Just herr, procHandle) <- SP.createProcess (
    SP.proc
      "wmctrl"
      ["-d"]){SP.std_in = SP.CreatePipe, SP.std_out = SP.CreatePipe, SP.std_err = SP.CreatePipe}
  hClose hin
  let r = C.pack "^(\\d+)\\s+\\*\\s+DG:\\s+(\\w+)\\s+.*"
  let f = do
        e <- hIsEOF hout
        if e
        then hClose hout >> hClose herr >> return Nothing
        else do
          ln <- hGetLine hout
          if ln =~ r :: Bool
          then  let (_, _, _, [ws, res]) = ln =~ r :: (String, String, String, [String])
                in hClose hout >> hClose herr >> return (Just (ws, res))
          else f
  fromJust <$> f

data XWin = XWin { wid:: WinID, title:: WinTitle, winclass :: T.Text }
getWindows :: WorkSpace -> IO [XWin]
getWindows ws =
  do
    (Just hin, Just hout, Just herr, procHandle) <- SP.createProcess  (
      SP.proc
        "wmctrl"
        ["-l", "-x"]){SP.std_in = SP.CreatePipe, SP.std_out = SP.CreatePipe, SP.std_err = SP.CreatePipe}
    hClose hin
    putStrLn ("^0x(\\w+)\\s+" ++ ws ++ "\\s+\\S+\\s+(.+)")
    let inWS = C.pack ("^0x(\\w+)\\s+" ++ ws ++ "\\s+(\\w+)\\.\\w+\\s+\\S+\\s+(.+)")
    let f acc = do
              e <- hIsEOF hout
              if e
              then hClose hout >> hClose herr >> return (reverse acc)
              else do
                ln <- hGetLine hout
                if ln =~ inWS :: Bool
                then  let (_, _, _, [r, c, ttl]) = ln =~ inWS :: (String, String, String, [String])
                      in f (XWin r (T.pack ttl) (T.pack c):acc)
                else f acc
    f []


strToInt :: String -> Int
strToInt x = read x

execStr :: String -> IO ()
execStr x = do
  (Just hin, Just hout, Just herr, procHandle) <- SP.createProcess  (
    SP.proc
      "/bin/bash"
      ["-c", x]){SP.std_in = SP.CreatePipe, SP.std_out = SP.CreatePipe, SP.std_err = SP.CreatePipe}
  o <- hGetContents hout
  putStrLn o
  e <- hGetContents herr
  putStrLn e
  hClose hin >> hClose hout >> hClose herr

charhint :: [a] -> Map Char a
charhint x = fromList $ zip "abcdefghijklmnopqrstuvwxyz" x

xwinlabel :: T.Text -> T.Text -> IO Gtk.Label
xwinlabel name txt = do
  l <- new Gtk.Label [ #label := txt ]
  ctxt <- widgetGetStyleContext l
  styleContextAddClass ctxt (T.concat ["wbtn_" , name])
  styleContextAddClass ctxt "wmjump_button"
  return l

goAhead :: Gtk.Entry -> Map Char XWin -> Map String String -> IO ()
goAhead en cwins wmap = do
  t <- Gtk.entryGetText en
  let usels = [Data.List.Split.split (condense . dropDelims $ oneOf ":,.") x |
               x <- Data.List.Split.split (condense . dropDelims $ oneOf " ") (T.unpack t)]
  sequence_ [execStr ("wmctrl -i -r 0x" ++ (wid $ cwins ! (head $ head iname)) ++ " -e 0," ++ (wmap ! head (tail iname))) | iname <- usels]
  Gtk.mainQuit


main :: IO ()
main = do
  homeDir <- getHomeDirectory
  (curWS,res) <- getCurrentWSAndRes
  putStrLn $ "desktop: " ++ curWS ++ "\nresolution: " ++ res
  wmap <- winsToMap <$> getGeomList res homeDir
  wins <- getWindows curWS
  let cwins = charhint wins
  print (length wins)
  putStrLn "" >> putStr "Available Keys: " >> sequence_ [ putStr (w ++ " ") | w <- keys wmap ]

  Gtk.init Nothing
  cssp <- cssProviderNew
  cssProviderLoadFromPath cssp $ T.concat [ T.pack homeDir , "/.wmjump/wmjump.css" ]

  mainwin <- new Gtk.Window [ #title := "WindowTiler" ]
  on mainwin #destroy Gtk.mainQuit
  scr <- widgetGetScreen mainwin
  Gtk.styleContextAddProviderForScreen scr cssp (fromIntegral STYLE_PROVIDER_PRIORITY_USER)
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #add mainwin box
  sequence_ [
    xwinlabel (winclass $ cwins ! c) (T.concat [T.singleton c , " : ", title (cwins ! c)]) >>= #add box
    | c <- keys cwins
    ]

  entry <- new Gtk.Entry []
  #add box entry
  on entry #activate (goAhead entry cwins wmap)

  #showAll mainwin

  Gtk.main

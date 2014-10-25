module Events where

import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import qualified Graphics.UI.GLFW as G

import qualified Model as M

setCallbacks :: TQueue M.Event -> G.Window -> IO ()
setCallbacks eventsChan win = do
    G.setErrorCallback (Just $ errorCallback eventsChan)
    G.setKeyCallback win (Just $ keyCallback eventsChan)
    G.setCursorPosCallback win (Just $ cursorPosCallback eventsChan)

errorCallback       :: TQueue M.Event -> G.ErrorCallback
keyCallback         :: TQueue M.Event -> G.KeyCallback
cursorPosCallback   :: TQueue M.Event -> G.CursorPosCallback

errorCallback tc e s =
    atomically $ writeTQueue tc $ M.EventError e s
keyCallback tc win k sc action mods =
    atomically $ writeTQueue tc $ M.EventKey win k sc action mods
cursorPosCallback tc win x y =
    atomically $ writeTQueue tc $ M.EventCursor x y

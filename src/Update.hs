module Update (update) where

import Control.Concurrent.STM (tryReadTQueue, atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import qualified Graphics.UI.GLFW as G

import qualified Model as M

-------------------------------------------------------------------------------

update :: M.App
update = processEvents

-------------------------------------------------------------------------------

processEvents :: M.App
processEvents = do
    tc <- asks M.envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
        Nothing -> return ()
        Just e -> do
            processEvent e
            processEvents

processEvent :: M.Event -> M.App
processEvent e = do
    win <- asks M.envWindow
    case e of
        (M.EventError _ _) -> liftIO $ G.setWindowShouldClose win True
        (M.EventKey _ G.Key'Escape _ G.KeyState'Pressed _) ->
            liftIO $ G.setWindowShouldClose win True
        _ -> return ()

-------------------------------------------------------------------------------

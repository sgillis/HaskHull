import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.State (runStateT)

import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as GL

import Window (withWindow)
import qualified Model as M
import qualified Events as E
import Update (update)
import View (draw)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 1280
        height = 720

    eventsChan <- newTQueueIO :: IO (TQueue M.Event)

    withWindow width height "HaskHull" $ \win -> do
        E.setCallbacks eventsChan win
        G.setCursorInputMode win G.CursorInputMode'Disabled
        GL.depthFunc GL.$= Just GL.Less
        GL.cullFace GL.$= Just GL.Front

        let env = M.Env
                { M.envEventsChan = eventsChan
                , M.envWindow     = win
                }
            state = M.State
                { M.viewer        = M.initialViewer
                }
        runApp env state

-------------------------------------------------------------------------------

runRST :: Monad m => M.RST r st m a -> r -> st -> m (a,st)
runRST rst r st = flip runStateT st . flip runReaderT r $ rst

runApp :: M.Env -> M.State -> IO ()
runApp env state = void $ runRST run env state

run :: M.App
run = do
    win <- asks M.envWindow

    liftIO $ do
        G.pollEvents

    update
    draw

    q <- liftIO $ G.windowShouldClose win
    unless q run

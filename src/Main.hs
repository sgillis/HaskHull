module Main (main) where

-------------------------------------------------------------------------------

import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, unless)
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.State.Strict (runStateT)
import Graphics.GLUtil.JuicyTextures

import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Events
import View
import Update
import Window (withWindow)
import Objects.Points (makePoints)
import Paths_HaskHull

-------------------------------------------------------------------------------

runRST :: Monad m => RST r st m a -> r -> st -> m (a,st)
runRST rst r st = flip runStateT st . flip runReaderT r $ rst

runApp :: Env -> State -> IO ()
runApp env state = void $ runRST run env state

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 1280
        height = 720

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "test" $ \win -> do
        setCallbacks eventsChan win
        G.setCursorInputMode win G.CursorInputMode'Disabled
        GL.depthFunc GL.$= Just GL.Less
        GL.cullFace GL.$= Just GL.Front
        GL.pointSize GL.$= 10
        GL.pointSmooth GL.$= GL.Enabled

        (fbWidth, fbHeight) <- G.getFramebufferSize win

        mpoints <- runMaybeT makePoints

        maybe
            (return ())
            (\points -> do
                let env = Env
                        { envEventsChan     = eventsChan
                        , envWindow         = win
                        }
                    state = State
                        { stateWindowWidth  = fbWidth
                        , stateWindowHeight = fbHeight
                        , points            = points
                        , viewer            = initialViewer
                        }
                runApp env state)
            mpoints

-------------------------------------------------------------------------------

run :: App
run = do
    win <- asks envWindow

    update
    draw

    liftIO $ do
        G.swapBuffers win
        G.pollEvents

    q <- liftIO $ G.windowShouldClose win
    unless q run

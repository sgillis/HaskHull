module View where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets)
import Control.Monad (forM_)
import Linear
import Linear.OpenGL()
import Graphics.GLUtil.Camera3D
import qualified Graphics.Rendering.OpenGL as GL

import Model
import Utils
import Graphics.Renderable
import Graphics.Object

draw :: App
draw = do
    p <- gets points
    v <- gets viewer
    w <- gets stateWindowWidth
    h <- gets stateWindowHeight

    liftIO $ do
        GL.clearColor GL.$= GL.Color4 0 0 0 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        drawPoints w h v p

        GL.flush

drawPoints :: Int -> Int -> Viewer -> Object -> IO ()
drawPoints w h player points =
    renderWith points $ \program -> do
        cameraLoc <- GL.get $ GL.uniformLocation program "camera"
        GL.uniform cameraLoc GL.$= camMatrix (cam player)
        projectionLoc <- GL.get $ GL.uniformLocation program "projection"
        GL.uniform projectionLoc GL.$= projection w h

cam :: Viewer -> Camera GL.GLfloat
cam player =
    panRad (realToFrac x) $
    tiltRad (realToFrac y) $
    dolly (v3toGL (position player)) $
    fpsCamera
    where x = horizontalAngle player
          y = verticalAngle player

projection :: Int -> Int -> M44 GL.GLfloat
projection w h = projectionMatrix 45 (realToFrac w/realToFrac h) 0.01 100

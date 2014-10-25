module View (draw) where

import Control.Monad.IO.Class (liftIO)
import Linear (M44)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil.Camera3D as C

import qualified Model as M
import Objects.Points

-------------------------------------------------------------------------------

draw :: M.App
draw = do
    liftIO $ do
        GL.clearColor GL.$= GL.Color4 1 1 1 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        makePoints

        GL.flush

cam :: Viewer -> Camera GL.GLfloat
cam viewer =
    C.panRad (realToFrac x) $
    C.tiltRad (realToFrac y) $
    C.dolly (v3ToGL (M.position viewer)) $
    C.fpsCamera
    where x = M.horizontalAngle viewer
          y = M.verticalAngle viewer

projection :: Int -> Int -> M44 GL.GLfloat
projection w h = projectionMatrix 45 (realToFrac w/realToFrac h) 0.01 100

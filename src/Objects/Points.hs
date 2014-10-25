module Objects.Points (makePoints) where

import Control.Monad.IO.Class (liftIO)

import qualified Graphics.Rendering.OpenGL as GL

import Paths_HaskHull
import Graphics.Object (makeObject, Object)
import Graphics.Buffers (ptrOffset)

makePoints :: IO ()
makePoints = do
    vertShaderPath <- liftIO $ getDataFileName "points_vertex.shader"
    fragShaderPath <- liftIO $ getDataFileName "points_fragment.shader"
    return ()

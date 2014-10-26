module Objects.Points (makePoints) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)

import qualified Graphics.Rendering.OpenGL as GL

import Paths_HaskHull
import Graphics.Object (makeObject, Object)
import Graphics.Buffers (ptrOffset)

makePoints :: MaybeT IO Object
makePoints = do
    vertShaderPath <- liftIO $ getDataFileName "vertex.shader"
    fragShaderPath <- liftIO $ getDataFileName "fragment.shader"
    makeObject
        [ (GL.VertexShader, vertShaderPath)
        , (GL.FragmentShader, fragShaderPath)
        ]
        vertexPositions
        [("position", GL.VertexArrayDescriptor 4 GL.Float 0 $ ptrOffset 0)]
        GL.Points 8
    where
        vertexPositions =
            [  1.0,  1.0,  1.0, 1.0
            ,  1.0,  1.0, -1.0, 1.0
            ,  1.0, -1.0,  1.0, 1.0
            ,  1.0, -1.0, -1.0, 1.0
            , -1.0,  1.0,  1.0, 1.0
            , -1.0,  1.0, -1.0, 1.0
            , -1.0, -1.0,  1.0, 1.0
            , -1.0, -1.0, -1.0, 1.0
            ]

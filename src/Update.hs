module Update (update) where

import Control.Concurrent.STM (tryReadTQueue, atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (modify, gets)
import Linear

import qualified Graphics.UI.GLFW as G

import Model

-------------------------------------------------------------------------------

update :: App
update = processEvents >> physics

-------------------------------------------------------------------------------

physics :: App
physics =
    modify $ \s -> s { viewer = viewerPhysics $ viewer s }

viewerPhysics :: Viewer -> Viewer
viewerPhysics viewer =
    let position' = (position viewer) ^+^ (0.01 *^ velocity viewer)
        velocity' = (velocity viewer) ^-^ (0.05 *^ velocity viewer)
    in viewer { position = position'
              , velocity = velocity' }

-------------------------------------------------------------------------------

processEvents :: App
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
        Nothing -> return ()
        Just e -> do
            processEvent e
            processEvents

processEvent :: Event -> App
processEvent e = do
    win <- asks envWindow
    case e of
        (EventError _ _) -> liftIO $ G.setWindowShouldClose win True
        (EventKey _ G.Key'Escape _ G.KeyState'Pressed _) ->
            liftIO $ G.setWindowShouldClose win True
        (EventKey _ G.Key'S _ _ _)         -> moveViewer (V3   0    0   1 )
        (EventKey _ G.Key'W _ _ _)         -> moveViewer (V3   0    0 (-1))
        (EventKey _ G.Key'A _ _ _)         -> moveViewer (V3   1    0   0 )
        (EventKey _ G.Key'D _ _ _)         -> moveViewer (V3 (-1)   0   0 )
        (EventKey _ G.Key'Space _ _ _)     -> moveViewer (V3   0    1   0 )
        (EventKey _ G.Key'LeftShift _ _ _) -> moveViewer (V3   0  (-1)  0 )
        (EventCursor x y) -> setViewerDirection x y
        _ -> return ()

-------------------------------------------------------------------------------

moveViewer :: V3 Double -> App
moveViewer input = do
    p <- gets viewer
    modify $ \s -> s { viewer = move input $ viewer s }

move :: V3 Double -> Viewer -> Viewer
move input viewer =
    let direction = moveDirection input viewer
    in  viewer { velocity = velocity viewer + direction }

moveDirection :: V3 Double -> Viewer -> V3 Double
moveDirection (V3 x _ z) viewer =
    let moveDir = normalize $ flatten $ direction viewer
        strafeDir = strafeDirection moveDir
        move = z *^ moveDir
        strafe = x *^ strafeDir
    in move + strafe

strafeDirection :: V3 Double -> V3 Double
strafeDirection direction =
    let quaternion = axisAngle (V3 0 1 0) (-pi/2)
        rotMat = fromQuaternion quaternion
    in rotMat !* direction

flatten :: V3 Double -> V3 Double
flatten (V3 x _ z) = V3 x 0 z

setViewerDirection :: Double -> Double -> App
setViewerDirection x y = do
    modify $ \s -> s { viewer = setDirection x y $ viewer s }

setDirection :: Double -> Double -> Viewer -> Viewer
setDirection x y viewer =
    viewer { horizontalAngle = (-x) / 1000
           , verticalAngle   = (-y) / 1000
           }

-------------------------------------------------------------------------------

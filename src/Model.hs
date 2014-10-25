module Model where

import Control.Concurrent.STM (TQueue)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Linear (V3(..))

import qualified Graphics.UI.GLFW as G

data Env = Env
    { envEventsChan     :: TQueue Event
    , envWindow         :: !G.Window
    }

data State = State
    { viewer            :: Viewer 
    }

data Event =
      EventError    !G.Error !String
    | EventKey      !G.Window !G.Key !Int !G.KeyState !G.ModifierKeys
    | EventCursor   !Double !Double
    deriving Show

type RST r st m = ReaderT r (StateT st m)
type App = RST Env State IO ()

type Position = V3 Double

data Viewer = Viewer
    { position          :: Position
    , horizontalAngle   :: Double
    , verticalAngle     :: Double
    }

initialViewer :: Viewer
initialViewer =
    Viewer { position        = V3 0 0 0
           , horizontalAngle = 0
           , verticalAngle   = 0
           }

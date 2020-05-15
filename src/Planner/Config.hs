module Planner.Config where
    
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity

data Configuration = Configuration {
    wheelBase       :: Double, -- ^ length between axels of vehicle
    maxSteer        :: Double,  -- ^ maximum steering angle in one direction
    nSteer          :: Int,    -- ^ number of steering angles
    motionResolution:: Double, -- ^ motion resolution 
    gridResolution  :: Double -- ^ size of grid squares

} deriving Show

class HasConfig env where
    configuration :: env -> Configuration

class (Monad m) => MonadConfig m where
    getConfig :: m Configuration

instance (Monad m, HasConfig env) => MonadConfig (ReaderT env m) where
    getConfig = asks configuration
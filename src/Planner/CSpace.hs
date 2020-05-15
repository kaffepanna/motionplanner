module Planner.CSpace where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map (Map, (!?))
import qualified Data.Map as Map

data Cell = Open | Closed
    deriving (Eq, Ord)

data CSpace = CSpace {
    grid :: Map (Int,Int,Int) Cell
}

class HasCSpace env where
    cspace :: env -> CSpace

class Monad m => MonadCSpace m where
    getCSpace :: m CSpace
    collision :: (Int,Int,Int) -> m Bool

instance (Monad m, HasCSpace env) => MonadCSpace (ReaderT env m) where
    getCSpace = asks cspace
    collision n = do
        cspace <- grid <$> getCSpace
        return $ maybe True (== Open) (cspace !? n) 
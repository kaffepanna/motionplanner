module Main where

import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map

import Render
import Planner
import Planner.Config 
import Planner.CSpace

import Debug.Trace
import Diagrams.Prelude (scale, scaleUToX, (#))
import Diagrams.Backend.SVG.CmdLine

cfg = Configuration {
        wheelBase        = 0.8,                           -- Metre, wheelbase
        maxSteer         = pi / 8,                        -- Radian, maximum steering angle in one direction
        nSteer           = 4,                             -- descreete steering angles to test
        motionResolution = (sqrt 2) * gridResolution cfg, -- Meter,
        gridResolution   = 0.5                            -- Meter
}

cs = CSpace {
        grid = Map.fromList $ -- grid coordinates
               [((x, y, o), Closed) | x<- [6,7], y <- [-7..7], o <- [0..40]]
}

env = Env cfg cs


start = (0.5, 0.5, 0)
goal =  (5.5, -3.4, pi)

xypath :: [Node] -> [(Double, Double)]
xypath = fmap (\(Node _ (x, y, _) _) -> (x , y))


main :: IO ()
main = do
        let (x, y, _) = start
        let plannerRun = flip runPlannerT
        plannerRun env $ do
           path <- generatePath start goal
           diaPath <- renderPath path
           diaGrid <- renderGrid 
           lift $ mapM_ print path
           lift $ mainWith $ (diaPath <> diaGrid) # scaleUToX 800

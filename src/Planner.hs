{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Planner where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Functor
import Vehicle

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Planner.Config
import Planner.CSpace

-- import Algorithm.Astar
import Algorithm.Astar (astarM)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Data.Hashable
import Data.Fixed
import Debug.Trace


data Node = Node {
    pos' :: (Int, Int, Int),
    pos  :: (Double, Double, Double),
    steer :: Double
} deriving (Ord, Show)

node :: (Monad m) => (Double, Double, Double) -> Double -> PlannerT m Node
node (x, y, o) a = let o' = o
                       a' = a
                    in Node <$> gridIndex (x, y, o') <*> pure (x, y, o') <*> pure a'

instance Eq Node where
    Node i1 _ _ == Node i2 _ _ = i1 == i2
                                                           
instance Hashable Node where
    hashWithSalt s (Node i _ _) = hashWithSalt s i

data Env = Env { config :: Configuration, _cspace :: CSpace }

newtype PlannerT m a = Config { unPlanner :: ReaderT Env m a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadConfig, MonadCSpace, MonadTrans)

type Planner = PlannerT Identity

instance HasConfig Env where 
    configuration = config

instance HasCSpace Env where
    cspace = _cspace

normalize a = mod' a (2*pi)

runPlannerT :: (Monad m) => PlannerT m a -> Env -> m a
runPlannerT = runReaderT . unPlanner

steerAngles :: (Monad m) => PlannerT m [Double]
steerAngles = do
    ms <- maxSteer <$> getConfig
    sr <- nSteer   <$> getConfig
    let step = ms / (fromIntegral sr)
    pure $ [-ms, (-ms+step)..(step-ms)] ++ [step, (step)..ms]

gridIndex :: (Monad m) => (Double, Double, Double) -> PlannerT m (Int, Int, Int)
gridIndex (x, y, o) = do
    res <- gridResolution <$> getConfig
    let x' = x / res
    let y' = y / res
    let o' =  2*pi*(normalize o)
    pure (round x', round y', round o')

nextMoves :: (Monad m) => Node -> PlannerT m [Node]
nextMoves (Node (x',y', o') (x,y, o) _) = do
    angles <- steerAngles
    d      <- motionResolution <$> getConfig
    l      <- wheelBase <$> getConfig

    poses <- forM angles $ \a ->
        node (nextPose (x,y,o) d l a) a

    filterM (\(Node idx _ _) -> collision idx) poses

cost :: (Monad m) => Node -> Node -> PlannerT m Double
cost (Node _ (x1, y1, o1) a1) (Node _ (x2, y2, o2) a2) =
    return $  distCart + distOr + distSteer
    where 
        distCart = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2
        distSteer = 2*pi*distAngle a1 a2
        distOr = pi*distAngle o1 o2
        distAngle a1 a2 = let a' = mod' (a1 - a2) 2*pi
                              b' = mod' (a2 - a1) 2*pi
                           in if a' < b' then a' else b'


generatePath :: (Monad m) => (Double, Double, Double) -- ^ start pose
             -> (Double, Double, Double) -- ^ end pose
             -> PlannerT m [Node]
generatePath (xs, ys, os) (xg, yg, og) = do
    let startNode = node (xs, ys, os) 0
    goalNode  <- node (xg, yg, og) 0

    path <- astarM (\n -> HashSet.fromList <$> nextMoves n)
                   cost
                   (\n -> cost goalNode n)
                   (\n -> pure $ goalNode == n)
                   startNode
    return $ reverse $ foldl (\l n -> n:l) [] path
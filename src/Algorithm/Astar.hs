{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algorithm.Astar (astar, astarM) where

import Data.Monoid ((<>), mappend)
import Data.Maybe (fromJust)
import Control.Monad (foldM, join)
import Data.Functor
import Data.Functor.Identity (runIdentity)

import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as Pq

import Data.Hashable

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Algorithm.Internal

import Debug.Trace

astar :: forall n h . (Eq n, Hashable n, Hashable h, Ord h, Num h)
      => (n -> HashSet n)     -- ^ Adjacent nodes function
      -> (n -> n -> h)        -- ^ distance function (d)
      -> (n -> h)             -- ^ heuristics function (g)
      -> (n -> Bool)          -- ^ function to deremin if node is goal
      -> n                    -- ^ starting node
      -> Seq n                -- ^ path
astar adj f g goal start = runIdentity $ astarM (pure . adj)
                                                (\n1 n2 -> pure $ f n1 n2)
                                                (pure . g)
                                                (pure . goal)
                                                (pure start)

astarM :: forall m n h . (Monad m, Hashable n, Hashable h, Eq n, Ord h, Num h)
      => (n -> m (HashSet n))   -- ^ Adjacent nodes function
      -> (n -> n -> m h)        -- ^ distance function (d)
      -> (n -> m h)             -- ^ heuristics function (g)
      -> (n -> m Bool)          -- ^ function to deremin if node is goal
      -> m n                    -- ^ starting node
      -> m (Seq n)              -- ^ path
astarM adj dist heur isGoal startM = do
    let closedSet = Set.empty :: HashSet n
    let paths     = Map.empty :: HashMap n n
    start <- startM
    openSet       <- flip Pq.singleton start <$> heur start
    gScores       <- Map.singleton start <$> dist start start
    fScores       <- Map.singleton start <$> heur start
    run openSet closedSet paths gScores fScores
    where
        run open closed paths gscore fscore = do
            goalReached <- isGoal current
            if | Pq.null open              -> pure Seq.empty
               | goalReached               -> pure $ constructPath current paths
               | Set.member current closed -> run open' closed paths gscore fscore
               | otherwise                 -> join $ run <$> open'' <*> closed' <*> paths' <*> gscore' <*> fscore'
            where Just (current, open') = Pq.minView open
                  closed'  = pure $ Set.insert current closed
                  adjacent = scoredAdjacent <&> Set.filter (\(n,g,f) -> not (Set.member n closed) && 
                                                                       (not (Map.member n gscore) || g < fromJust (Map.lookup n gscore)))
                  paths'   = foldl (\p (n,_,_) -> Map.insert n current p) paths <$> adjacent
                  gscore'  = foldl (\m (n,g,_) -> Map.insert n g m) gscore      <$> adjacent
                  fscore'  = foldl (\m (n,_,f) -> Map.insert n f m) fscore      <$> adjacent
                  open''   = foldl (\q (n,g,f) -> Pq.insert (g + f) n q) open'  <$> adjacent

                  scoredAdjacent = Set.fromList <$> (mapM (\an -> do
                                       g <- dist current an <&> (+) (fromJust (Map.lookup current gscore))
                                       f <- heur an         <&> (+) (fromJust (Map.lookup current fscore))
                                       return (an, g, f)) =<< Set.toList <$> adj current)

module Algorithm.Internal (constructPath) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

import Data.Maybe (fromJust)

constructPath node paths
    | Map.member node paths = constructPath (fromJust . Map.lookup node $ paths) paths >< Seq.singleton node
    | otherwise = Seq.singleton node
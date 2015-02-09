module Network.Traffic.Object.Enumerator
       ( enumerateByApplication
       ) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Network.Traffic.Object.Types

-- | Enumerate all kind of applications, and how may of each
-- application there are.
enumerateByApplication :: ObjectVector -> M.Map (Maybe Application) Int
enumerateByApplication = enumerateByField application

-- | Present an enumeration of the given field type, and how many of
-- each kind that are found.
enumerateByField :: Ord a => (Object -> a) -> ObjectVector ->  M.Map a Int
enumerateByField extractor =
  V.foldl' countObject M.empty
  where
    countObject acc obj = M.insertWithKey incCounter (extractor obj) 1 acc
    incCounter _ _ v = v + 1

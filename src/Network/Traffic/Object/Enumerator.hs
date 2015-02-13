module Network.Traffic.Object.Enumerator
       ( Enumeration
       , enumerateByTransport
       , enumerateByApplication
       , quantifyEnumeration
       ) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import GHC.Int (Int64)
import Network.Traffic.Object.Types

type Enumeration a = M.Map a Int64

-- | Enumerate all kind of transport, and how many of each transport
-- there are.
enumerateByTransport :: ObjectVector -> Enumeration (Maybe Transport)
enumerateByTransport = enumerateByField transport

-- | Enumerate all kind of applications, and how many of each
-- application there are.
enumerateByApplication :: ObjectVector -> Enumeration (Maybe Application)
enumerateByApplication = enumerateByField application

quantifyEnumeration :: Show a => Enumeration a
                       -> (Int64, [(String, Int, Float)])
quantifyEnumeration enumeration =
    let totalCount = M.foldl' (+) 0 enumeration
    in (totalCount, [])

-- | Present an enumeration of the given field type, and how many of
-- each kind that are found.
enumerateByField :: Ord a => (Object -> a) -> ObjectVector -> Enumeration a
enumerateByField extractor =
  V.foldl' countObject M.empty
  where
    countObject acc obj = M.insertWithKey incCounter (extractor obj) 1 acc
    incCounter _ _ v = v + 1

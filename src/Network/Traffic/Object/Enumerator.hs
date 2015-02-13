{-# LANGUAGE OverloadedStrings #-}
module Network.Traffic.Object.Enumerator
       ( Enumeration
       , enumerateByTransport
       , enumerateByApplication
       , quantifyEnumeration
       ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Int (Int64)
import Network.Traffic.Object.Types

type Enumeration a = M.Map a Int64
type Quantification = (Int64, [(Text, Int64, Float)])

-- | Enumerate all kind of transport, and how many of each transport
-- there are.
enumerateByTransport :: ObjectVector -> Enumeration (Maybe Transport)
enumerateByTransport = enumerateByField transport

-- | Enumerate all kind of applications, and how many of each
-- application there are.
enumerateByApplication :: ObjectVector -> Enumeration (Maybe Application)
enumerateByApplication = enumerateByField application

-- | Quantify an enumeration.
quantifyEnumeration :: Show a => Enumeration a -> Quantification
quantifyEnumeration enumeration =
    let totalCount = M.foldl' (+) 0 enumeration
        quantList  = M.foldlWithKey' (\acc key value ->
                                       ("hepp", value, 0.0):acc) [] enumeration
    in (totalCount, quantList)

-- | Present an enumeration of the given field type, and how many of
-- each kind that are found.
enumerateByField :: Ord a => (Object -> a) -> ObjectVector -> Enumeration a
enumerateByField extractor =
  V.foldl' countObject M.empty
  where
    countObject acc obj = M.insertWithKey incCounter (extractor obj) 1 acc
    incCounter _ _ v = v + 1

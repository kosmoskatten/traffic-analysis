{-# LANGUAGE OverloadedStrings #-}
module Network.Traffic.Object.Enumerator
       ( Enumeration
       , Quantification
       , enumerateByTransport
       , enumerateByApplication
       , enumerateByFunctionality
       , enumerateByServiceProvider
       , enumerateByClientApp
       , quantifyEnumeration
       ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Int (Int64)
import Network.Traffic.Object.Types

type Enumeration = M.Map Text Int64
type Quantification = (Int64, [(Text, Int64, Double)])

-- | Enumerate all kind of transport, and how many of each transport
-- there are.
enumerateByTransport :: ObjectVector -> Enumeration
enumerateByTransport = enumerateByField transport

-- | Enumerate all kind of applications, and how many of each
-- application there are.
enumerateByApplication :: ObjectVector -> Enumeration
enumerateByApplication = enumerateByField application

-- | Enumerate all kind of functionality, and how many of each
-- function there are.
enumerateByFunctionality :: ObjectVector -> Enumeration
enumerateByFunctionality = enumerateByField functionality

-- | Enumerate all kind of service providers, and how many of each
-- provider there are.
enumerateByServiceProvider :: ObjectVector -> Enumeration
enumerateByServiceProvider = enumerateByField serviceProvider

-- | Enumerate all kind of client apps, and how many of each apps
-- there are.
enumerateByClientApp :: ObjectVector -> Enumeration
enumerateByClientApp = enumerateByField clientApp

-- | Quantify an enumeration.
quantifyEnumeration :: Enumeration -> Quantification
quantifyEnumeration enumeration =
    let totalCount = M.foldl' (+) 0 enumeration
        quantList  =
          M.foldlWithKey' (\acc key value ->
                            (key, value, value `perc` totalCount):acc)
          [] enumeration
        quantList' = sortBy (\(_, c1, _) (_, c2, _) ->
                              -- Sort in descending order.
                              c2 `compare` c1) quantList
    in (totalCount, quantList')
  where
    perc :: Int64 -> Int64 -> Double
    {-# INLINE perc #-}
    perc _ 0 = 0.0
    perc x y =
      let x' = fromIntegral x
          y' = fromIntegral y
      in (x' / y') * 100.0

-- | Present an enumeration of the given field type, and how many of
-- each kind that are found.
enumerateByField :: (Ord a, Show a)
                    => (Object -> Maybe a) -> ObjectVector -> Enumeration
enumerateByField extractor = V.foldl' countObject M.empty
  where
    countObject :: Enumeration -> Object -> Enumeration
    countObject enum obj =
      M.insertWithKey incCounter (toText $ extractor obj) 1 enum

    incCounter :: Text -> Int64 -> Int64 -> Int64
    {-# INLINE incCounter #-}
    incCounter _ _ v = v + 1

    toText :: Show a => Maybe a -> Text
    {-# INLINE toText #-}
    toText (Just x) = T.pack $ show x
    toText Nothing = "Unspecified"


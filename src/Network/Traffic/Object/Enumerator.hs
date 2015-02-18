{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Network.Traffic.Object.Enumerator
       ( Enumeration
       , EnumerationTarget (..)
       , Quantification
       , printable
       , enumerateBy
       , toString
       , quantifyEnumeration
       ) where

import Control.Monad.Par
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Int (Int64)
import Network.Traffic.Object.Counter ( Counter (..)
                                      , add
                                      , fromObject )
import qualified Network.Traffic.Object.Counter as C
import Network.Traffic.Object.Types (ObjectVector, Object (..))
import Text.Printf (printf)

type Enumeration = M.Map Text Counter
type Quantification = (Counter, [(Text, Counter)])

data EnumerationTarget = Transport
                       | Application 
                       | Functionality
                       | ServiceProvider
                       | ClientApp
                       | TerminalType
    deriving (Show)

printable :: EnumerationTarget -> ObjectVector -> String
printable target = toString . quantifyEnumeration . enumerateBy target

-- | Enumerate an object vector in its target. Each instance value of
-- the target is accounted from the object vector.
enumerateBy :: EnumerationTarget -> ObjectVector -> Enumeration
enumerateBy Transport = enumerateByField transport
enumerateBy Application = enumerateByFieldPar application
enumerateBy Functionality = enumerateByField functionality
enumerateBy ServiceProvider = enumerateByField serviceProvider
enumerateBy ClientApp = enumerateByField clientApp
enumerateBy TerminalType = enumerateByField terminalType

-- | Quantify an enumeration.
quantifyEnumeration :: Enumeration -> Quantification
quantifyEnumeration enum = 
    let totalCount = M.foldl' add C.empty enum
    in (totalCount, M.toList enum)

toString :: Quantification -> String
toString (total, quantList) =
    unlines $ map objString quantList
    where
      objString :: (Text, Counter) -> String
      objString (text, counter) =
          let l1 = printf "Item %s count: %ld (%.2f procent of total)."
                   (T.unpack text) (C.count counter) 
                   (C.count counter `perc` C.count total)
              l2 = printf " DL bytes: %ld (%.2f procent of total)."
                   (C.downlinkBytes counter) 
                   (C.downlinkBytes counter `perc` C.downlinkBytes total)
              l3 = printf " DL packets: %ld (%.2f procent of total)."
                   (C.downlinkPackets counter)
                   (C.downlinkPackets counter `perc` C.downlinkPackets total)
              l4 = printf " UL bytes: %ld (%.2f procent of total)."
                   (C.uplinkBytes counter) 
                   (C.uplinkBytes counter `perc` C.uplinkBytes total)
              l5 = printf " UL packets: %ld (%.2f procent of total)."
                   (C.uplinkPackets counter)
                   (C.uplinkPackets counter `perc` C.uplinkPackets total)
          in unlines [l1, l2, l3, l4, l5]

      perc :: Int64 -> Int64 -> Double
      {-# INLINE perc #-}
      perc _ 0 = 0.0
      perc !x !y =
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
        let counter = fromObject obj
        in M.insertWithKey incCounter (toText $ extractor obj) counter enum

    incCounter :: Text -> Counter -> Counter -> Counter
    {-# INLINE incCounter #-}
    incCounter _ = add

    toText :: Show a => Maybe a -> Text
    {-# INLINE toText #-}
    toText (Just x) = T.pack $ show x
    toText Nothing = "Unspecified"

enumerateByFieldPar :: (Ord a, Show a)
                       => (Object -> Maybe a) -> ObjectVector -> Enumeration
enumerateByFieldPar extractor = go
    where
      go :: ObjectVector -> Enumeration
      go objects 
          | V.length objects > 20000000 =
              runPar $ do
                let pivot = V.length objects `div` 2
                    parts = V.splitAt pivot objects
                l <- spawnP $ go (fst parts)
                r <- spawnP $ go (snd parts)
                ll <- get l
                rr <- get r
                return $ M.unionWith add ll rr
          | otherwise = enumerateByField extractor objects

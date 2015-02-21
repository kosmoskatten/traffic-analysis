module Network.Traffic.Object.Filter
       ( FilterFunc (..)
       , filterObjects
       ) where

import qualified Data.Vector as V
import Network.Traffic.Object.Types ( Object, ObjectVector )

newtype FilterFunc = FilterFunc (Object -> Bool)

instance Show FilterFunc where
  show _ = "(Object -> Bool)"

filterObjects :: Maybe FilterFunc -> ObjectVector -> ObjectVector
filterObjects Nothing objs               = objs
filterObjects (Just (FilterFunc f)) objs = V.filter f objs


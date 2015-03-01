{-# LANGUAGE RecordWildCards #-}
module Network.Traffic.Object.Timeline
       ( Timeline
       , Duration (..)
       , timeline
       , totalPlaytime
       ) where

import Control.Applicative ((<$>))
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import Network.Traffic.Object.Types ( Object (..)
                                    , ObjectVector )

type Timeline = V.Vector (ObjectVector, ObjectVector)
data Duration = Seconds !Float
              | Milliseconds !Float
              | Microseconds !Float
  deriving (Show)

timeline :: ObjectVector -> Duration -> Timeline
timeline objects resolution =
  let totalPlaytime' = totalPlaytime objects
      unit           = toSeconds resolution
      units'         = units totalPlaytime' unit
  in runST $ do
    vector <- newSTRef objects
    live   <- newSTRef V.empty
    V.generateM units' $ \index -> do
      let nextStartTime = unit `mult` (index + 1)
          
      (starters, remaining) <-
        V.partition (startIfLessThan nextStartTime) <$> readSTRef vector        
      writeSTRef vector remaining

      live' <- V.filter (stopIfLessThan nextStartTime) <$> readSTRef live
      writeSTRef live $ live' V.++ starters      
      
      return (starters, live')

startIfLessThan :: Float -> Object -> Bool
startIfLessThan t Object {..} = timestamp < t

stopIfLessThan :: Float -> Object -> Bool
stopIfLessThan t Object {..} = (timestamp + duration) >= t

totalPlaytime :: ObjectVector -> Duration
totalPlaytime = Seconds . V.foldl' greatest 0
  where
    greatest :: Float -> Object -> Float
    {-# INLINE greatest #-}
    greatest acc Object {..} = max acc (timestamp + duration)

units :: Duration -> Duration -> Int
units (Seconds dur) (Seconds res) = ceiling $ dur / res
units dur res = units (toSeconds dur) (toSeconds res)

toSeconds :: Duration -> Duration
toSeconds (Microseconds us) = Seconds (us * 0.000001)
toSeconds (Milliseconds ms) = Seconds (ms * 0.001)
toSeconds s                 = s

mult :: Duration -> Int -> Float
mult (Seconds s)       = mult' s
mult (Milliseconds ms) = mult' ms
mult (Microseconds us) = mult' us

mult' :: Float -> Int -> Float
{-# INLINE mult' #-}
mult' t f = t * fromIntegral f


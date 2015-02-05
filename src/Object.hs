{-# LANGUAGE OverloadedStrings #-}
module Object
       ( Object (..)
       , decodeObjects
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Csv ( FromField (parseField)
                , FromRecord (parseRecord)
                , HasHeader (NoHeader)
                , DecodeOptions (..)
                , (.!)
                , decodeWith
                , defaultDecodeOptions )
import qualified Data.Vector as V

-- | Transport protocol.
data Transport = TCP | UDP
    deriving Show

-- | Application protocol.
data Application = HTTP
    deriving Show

-- | An object is the aggregation of data packets from a continous
-- transmission flow of a single IP flow.
data Object = Object
              { timestamp :: !Float -- Start of the object's
                                    -- transmission since measurement
                                    -- start. Microsecond precision.
              , duration  :: !Float -- In seconds.
              , transport :: !Transport -- Transport protocol.
              , ulPackets :: !Int -- Number of uplink packets.
              , dlPackets :: !Int -- Number of downlink packets.
              , ulBytes   :: !Integer -- Number of uplink bytes.
              , dlBytes   :: !Integer -- Number of downlink bytes.
              , flowId    :: !LBS.ByteString -- 64 bit flow id.
              , application :: !(Maybe Application) -- Application protocol.
              }
    deriving Show

instance FromField Transport where
  parseField "TCP" = return TCP
  parseField "UDP" = return UDP
  parseField _     = mzero

instance FromField Application where
  parseField "HTTP" = return HTTP
  parseField _      = mzero

instance FromRecord Object where
  parseRecord v
    | V.length v == 9 = Object <$> v .! 0
                               <*> v .! 1
                               <*> v .! 2
                               <*> v .! 3
                               <*> v .! 4
                               <*> v .! 5
                               <*> v .! 6
                               <*> v .! 7
                               <*> v .! 8
    | otherwise       = mzero

-- | Decode the bytestring for objects. The objects are expected to be
-- separated on a new line where each field separated by a bar '|'.
decodeObjects :: LBS.ByteString -> Either String (V.Vector Object)
decodeObjects lbs =
  let decodeOptions = defaultDecodeOptions
        { decDelimiter = fromIntegral (ord '|')
        }
  in decodeWith decodeOptions NoHeader lbs


  

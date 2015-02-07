module Network.Traffic.Object.Reader (decodeObjects) where

import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Csv ( HasHeader (NoHeader)
                , DecodeOptions (..)
                , decodeWith
                , defaultDecodeOptions )
import Network.Traffic.Object.Types (ObjectVector)

-- | Decode the bytestring for objects. The objects are expected to be
-- separated on a new line where each field separated by a bar '|'.
decodeObjects :: LBS.ByteString -> Either String ObjectVector
decodeObjects lbs =
  let decodeOptions = defaultDecodeOptions
        { decDelimiter = fromIntegral (ord '|')
        }
  in decodeWith decodeOptions NoHeader lbs

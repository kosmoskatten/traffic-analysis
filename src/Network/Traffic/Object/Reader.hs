{-# LANGUAGE BangPatterns #-}
module Network.Traffic.Object.Reader 
    ( decodeObjects
    , decodeObjectsPar
    ) where

import Control.Monad.Par
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Csv ( HasHeader (NoHeader)
                , DecodeOptions (..)
                , decodeWith
                , defaultDecodeOptions )
import qualified Data.Vector as V
import Network.Traffic.Object.NewlineSplitter (splitAtNextNewline)
import Network.Traffic.Object.Types (ObjectVector)

-- | Decode the bytestring for objects. The objects are expected to be
-- separated on a new line where each field separated by a bar '|'.
decodeObjects :: LBS.ByteString -> Either String ObjectVector
decodeObjects = decodeObjects' decodeOptions

decodeObjectsPar :: LBS.ByteString -> Either String ObjectVector
decodeObjectsPar = go decodeOptions
    where
      go :: DecodeOptions -> LBS.ByteString -> Either String ObjectVector
      go !options !lbs 
          | LBS.length lbs > 2000000 = 
              runPar $ do                                          
                let parts = splitAtNextNewline (LBS.length lbs `div` 2) lbs
                l <- spawnP $ go options (fst parts)
                r <- spawnP $ go options (snd parts)
                ll <- get l
                rr <- get r
                case ll of
                  Left err   -> return $ Left err
                  Right left ->
                      case rr of
                        Left err    -> return $ Left err
                        Right right -> return $ Right (left V.++ right)

          | otherwise                 = decodeObjects' options lbs

decodeObjects' :: DecodeOptions -> LBS.ByteString -> Either String ObjectVector
decodeObjects' options = decodeWith options NoHeader

decodeOptions :: DecodeOptions
decodeOptions = defaultDecodeOptions { decDelimiter = fromIntegral $ ord '|'}

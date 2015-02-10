module Network.Traffic.Object.NewlineSplitter
       (splitAtNextNewline
       ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Int (Int64)

splitAtNextNewline :: Int64
                   -> LBS.ByteString
                   -> (LBS.ByteString, LBS.ByteString)
splitAtNextNewline startIndex lbs = go startIndex $ LBS.length lbs
  where
    go :: Int64 -> Int64 -> (LBS.ByteString, LBS.ByteString)
    {-# INLINE go #-}
    go index end
      | index >= end                = LBS.splitAt index lbs
      | LBS.index lbs index == '\n' = LBS.splitAt (index + 1) lbs
      | otherwise                   = go (index + 1) end

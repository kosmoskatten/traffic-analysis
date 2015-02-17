{-# LANGUAGE RecordWildCards #-}
module Network.Traffic.Object.Counter
       ( Counter (..)
       , fromObject
       , add
       , empty
       ) where

import GHC.Int (Int64)
import Network.Traffic.Object.Types (Object (..))

-- | Aggregation counter for object data.
data Counter =
  Counter { count           :: {-# UNPACK #-} !Int64
          , uplinkPackets   :: {-# UNPACK #-} !Int64
          , downlinkPackets :: {-# UNPACK #-} !Int64
          , uplinkBytes     :: {-# UNPACK #-} !Int64
          , downlinkBytes   :: {-# UNPACK #-} !Int64
          }
    deriving (Show)

-- | Initialize a counter from an object.
fromObject :: Object -> Counter
{-# INLINE fromObject #-}
fromObject Object {..} =
  Counter { count           = 1
          , uplinkPackets   = ulPackets
          , downlinkPackets = dlPackets
          , uplinkBytes     = ulBytes
          , downlinkBytes   = dlBytes
          }

-- | Add two counters together.
add :: Counter -> Counter -> Counter
{-# INLINE add #-}
add c1 c2 =
  Counter { count           = count c1 + count c2
          , uplinkPackets   = uplinkPackets c1 + uplinkPackets c2
          , downlinkPackets = downlinkPackets c1 + downlinkPackets c2
          , uplinkBytes     = uplinkBytes c1 + uplinkBytes c2
          , downlinkBytes   = downlinkBytes c1 + downlinkBytes c2
          }

empty :: Counter
empty =
    Counter { count           = 0
            , uplinkPackets   = 0
            , downlinkPackets = 0
            , uplinkBytes     = 0
            , downlinkBytes   = 0 }

{-# LANGUAGE OverloadedStrings #-}
module Network.Traffic.Object.Types
       ( Object (..)
       , ObjectVector
       , Transport (..)
       , Application (..)
       , Encapsulation (..)
       , Encryption (..)
       , ServiceProvider (..)
       , ClientApp (..)
       , TerminalType (..)
       , Vendor (..)
       , OS (..)
       , Direction (..)
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv ( FromField (parseField)
                , FromRecord (parseRecord)
                , (.!) )
import qualified Data.Vector as V

type ObjectVector = V.Vector Object

-- | Transport protocol.
data Transport = ICMP | TCP | UDP
    deriving (Eq, Ord, Show)

-- | Application protocol.
data Application = BitTorrent | C2DM | DNS | HTTP | ICMP_ | IMAP
                 | NTP | POP3 | SMTP | SIP | XMPP
    deriving (Eq, Ord, Show)

-- | Functionality description.
data Functionality = Advertisement | Audio | CloudStorage
                   | Email | FileDownload | FileSharing
                   | IMAndPrecense | MMS | SocialNetworking
                   | System | Video | WebBrowsing
    deriving (Eq, Ord, Show)

-- | Encapsulation.
data Encapsulation = Encapsulation
    deriving (Eq, Ord, Show)

data Encryption = TLS_SSL
    deriving (Eq, Ord, Show)

-- | Service provider.
data ServiceProvider = Amazon | AdMob | Facebook | Google | GooglePlay | Netflix
                     | Microsoft | Pandora | P2P | Slacker | StreamTheWorld
                     | Twitter | Yahoo | YouTube
    deriving (Eq, Ord, Show)

-- | Client Application.
data ClientApp = AndroidMediaPlayer | GooglePlayApp | GTalk
               | InternetExplorer | WindowsMediaPlayer
               | YouTubePlayer | Zune
    deriving (Eq, Ord, Show)

-- | Terminal type.
data TerminalType = Handheld | M2M | PC | Router | Tablet
    deriving (Eq, Ord, Show)

-- | Vendor name.
newtype Vendor = Vendor BS.ByteString
    deriving (Eq, Ord, Show)

-- | Operating system.
data OS = OS BS.ByteString
    deriving (Eq, Ord, Show)

-- Direction.
data Direction = Downlink | Uplink
    deriving (Eq, Ord, Show)

-- | An object is the aggregation of data packets from a continous
-- transmission flow of a single IP flow.
data Object = 
    Object { timestamp       :: !Float -- Start of the object's
                                       -- transmission since measurement
                                       -- start. Microsecond precision.
           , duration        :: !Float -- In seconds.
           , transport       :: !Transport
           , ulPackets       :: !Int -- Number of uplink packets.
           , dlPackets       :: !Int -- Number of downlink packets.
           , ulBytes         :: !Integer -- Number of uplink bytes.
           , dlBytes         :: !Integer -- Number of downlink bytes.
           , flowId          :: !LBS.ByteString -- 64 bit flow id.
           , application     :: !(Maybe Application)
           , functionality   :: !(Maybe Functionality)
           , encapsulation   :: !(Maybe Encapsulation)
           , encryption      :: !(Maybe Encryption)
           , serviceProvider :: !(Maybe ServiceProvider)
           , clientApp       :: !(Maybe ClientApp)
           , anonId          :: !LBS.ByteString -- Anonymized user id.
           , terminalType    :: !(Maybe TerminalType)
           , vendor          :: !Vendor
           , os              :: !(Maybe OS)
           , direction       :: !Direction -- Direction of the first packet.
           }
    deriving Show

instance FromField Transport where
  parseField "ICMP" = return ICMP
  parseField "TCP"  = return TCP
  parseField "UDP"  = return UDP
  parseField _      = mzero

instance FromField Application where
  parseField "Android C2DM" = return C2DM
  parseField "BitTorrent"   = return BitTorrent
  parseField "DNS"          = return DNS
  parseField "HTTP"         = return HTTP
  parseField "ICMP"         = return ICMP_
  parseField "IMAP"         = return IMAP
  parseField "NTP"          = return NTP
  parseField "POP3"         = return POP3
  parseField "SMTP"         = return SMTP
  parseField "SIP"          = return SIP
  parseField "XMPP"         = return XMPP
  parseField _              = mzero

instance FromField Functionality where
  parseField "advertisement"     = return Advertisement
  parseField "audio"             = return Audio
  parseField "cloud-storage"     = return CloudStorage
  parseField "email"             = return Email
  parseField "file-download"     = return FileDownload
  parseField "file sharing"      = return FileSharing
  parseField "IM and presence"   = return IMAndPrecense
  parseField "MMS"               = return MMS
  parseField "social networking" = return SocialNetworking
  parseField "system"            = return System
  parseField "video"             = return Video
  parseField "web browsing"      = return WebBrowsing
  parseField _                   = mzero

instance FromField Encapsulation where
  parseField _ = mzero

instance FromField Encryption where
  parseField "TLS-SSL" = return TLS_SSL
  parseField _         = mzero

instance FromField ServiceProvider where
  parseField "Amazon"         = return Amazon
  parseField "AdMob"          = return AdMob
  parseField "Facebook"       = return Facebook
  parseField "Google"         = return Google
  parseField "Google Play"    = return GooglePlay
  parseField "Microsoft"      = return Microsoft
  parseField "Netflix"        = return Netflix
  parseField "Pandora"        = return Pandora
  parseField "P2P"            = return P2P
  parseField "Slacker"        = return Slacker
  parseField "StreamTheWorld" = return StreamTheWorld
  parseField "Twitter"        = return Twitter
  parseField "Yahoo"          = return Yahoo
  parseField "YouTube"        = return YouTube
  parseField _                = mzero

instance FromField ClientApp where
  parseField "Android Media Player" = return AndroidMediaPlayer
  parseField "Google Play"          = return GooglePlayApp
  parseField "GTalk"                = return GTalk
  parseField "Internet Explorer"    = return InternetExplorer
  parseField "Windows Media Player" = return WindowsMediaPlayer
  parseField "YouTube-player"       = return YouTubePlayer
  parseField "Zune"                 = return Zune
  parseField _                      = mzero

instance FromField TerminalType where
  parseField "HANDHELD" = return Handheld
  parseField "M2"       = return M2M
  parseField "PC"       = return PC
  parseField "ROUTER"   = return Router
  parseField "TABLET"   = return Tablet
  parseField _          = mzero

instance FromField Vendor where
  parseField = return . Vendor

instance FromField OS where
  parseField = return . OS

instance FromField Direction where
  parseField "downlink" = return Downlink
  parseField "uplink"   = return Uplink
  parseField _          = mzero
  
instance FromRecord Object where
  parseRecord v
    | V.length v == 19 = Object <$> v .!  0
                                <*> v .!  1
                                <*> v .!  2
                                <*> v .!  3
                                <*> v .!  4
                                <*> v .!  5
                                <*> v .!  6
                                <*> v .!  7
                                <*> v .!  8
                                <*> v .!  9
                                <*> v .! 10
                                <*> v .! 11
                                <*> v .! 12
                                <*> v .! 13
                                <*> v .! 14
                                <*> v .! 15
                                <*> v .! 16
                                <*> v .! 17
                                <*> v .! 18
    | otherwise       = mzero



  

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Network.Traffic.Object.Types
       ( EnumerationTarget (..)
       , Object (..)
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
       , maybeRead
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.DeepSeq (NFData())
import Control.Monad (mzero)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv ( FromField (parseField)
                , FromRecord (parseRecord)
                , (.!) )
import qualified Data.Vector as V
import GHC.Generics (Generic())
import GHC.Int (Int64)

-- | Enumeration target.
data EnumerationTarget = Transport
                       | Application 
                       | Functionality
                       | ServiceProvider
                       | ClientApp
                       | TerminalType
    deriving (Bounded, Enum, Read, Show)

type ObjectVector = V.Vector Object

-- | Transport protocol.
data Transport = ICMP | IGMP | IPv6 | TCP | UDP | VISA
    deriving (Eq, Generic, Ord, Read, Show)

-- | Application protocol.
data Application = ApplePushNotificationService | BitTorrent | C2DM 
                 | DHCP | DNS | FTP | HTTP | ICMP_ | IGMP_ 
                 | IMAP | NTP | PPStream 
                 | POP3 | QVOD | RTMP | RTP | RTSP | SMTP | SIP 
                 | Spotify | UPnP | Windows | XBOX | XMPP
    deriving (Eq, Generic, Ord, Show)

-- | Functionality description.
data Functionality = Advertisement | Audio | CloudStorage
                   | Email | FileDownload | FileSharing | Gaming
                   | IMAndPrecense | InstantMessaging | Maps 
                   | Media | MMS | PhotoSharing | SoftwareUpdate
                   | SocialNetworking | System 
                   | Video | Weather | WebBrowsing
    deriving (Eq, Generic, Ord, Show)

-- | Encapsulation.
newtype Encapsulation = Encapsulation BS.ByteString
    deriving (Eq, Generic, Ord, Show)

newtype Encryption = Encryption BS.ByteString
    deriving (Eq, Generic, Ord, Show)

-- | Service provider.
data ServiceProvider = Amazon | Facebook | Google | GooglePlay | Netflix
                     | Microsoft | P2P | Twitter | Yahoo | YouTube
                     | Service !BS.ByteString
    deriving (Eq, Generic, Ord, Show)

-- | Client Application.
data ClientApp = AndroidMediaPlayer | InternetExplorer | TwitterApp
               | WindowsMediaPlayer | YouTubePlayer
               | App !BS.ByteString
    deriving (Eq, Generic, Ord, Show)

-- | Terminal type.
data TerminalType = Handheld | M2M | PC | Router | Tablet
    deriving (Eq, Generic, Ord, Show)

-- | Vendor name.
newtype Vendor = Vendor BS.ByteString
    deriving (Eq, Generic, Ord, Show)

-- | Operating system.
data OS = OS BS.ByteString
    deriving (Eq, Generic, Ord, Show)

-- Direction.
data Direction = Downlink | Uplink
    deriving (Eq, Generic, Ord, Show)

-- | An object is the aggregation of data packets from a continous
-- transmission flow of a single IP flow.
data Object = 
    Object { timestamp       :: !Float -- Start of the object's
                                       -- transmission since measurement
                                       -- start. Microsecond precision.
           , duration        :: !Float -- In seconds.
           , transport       :: !(Maybe Transport)
           , ulPackets       :: !Int64 -- Number of uplink packets.
           , dlPackets       :: !Int64 -- Number of downlink packets.
           , ulBytes         :: !Int64 -- Number of uplink bytes.
           , dlBytes         :: !Int64 -- Number of downlink bytes.
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
    deriving (Generic, Show)

instance FromField Transport where
  parseField "ICMP" = return ICMP
  parseField "IGMP" = return IGMP
  parseField "IPv6" = return IPv6
  parseField "TCP"  = return TCP
  parseField "UDP"  = return UDP
  parseField "VISA" = return VISA
  parseField _      = mzero

instance FromField Application where
  parseField "Android_C2DM" = return C2DM
  parseField "Android C2DM" = return C2DM
  parseField "Apple_Push_Notification_Service" = 
      return ApplePushNotificationService
  parseField "BitTorrent"   = return BitTorrent
  parseField "DHCP"         = return DHCP
  parseField "DNS"          = return DNS
  parseField "FTP"          = return FTP
  parseField "HTTP"         = return HTTP
  parseField "ICMP"         = return ICMP_
  parseField "IGMP"         = return IGMP_
  parseField "IMAP"         = return IMAP
  parseField "NTP"          = return NTP
  parseField "PPStream"     = return PPStream
  parseField "POP3"         = return POP3
  parseField "QVOD"         = return QVOD
  parseField "RTMP"         = return RTMP
  parseField "RTP"          = return RTP
  parseField "RTSP"         = return RTSP
  parseField "SMTP"         = return SMTP
  parseField "SIP"          = return SIP
  parseField "Spotify"      = return Spotify
  parseField "UPnP"         = return UPnP
  parseField "Windows"      = return Windows
  parseField "xbox"         = return XBOX
  parseField "XMPP"         = return XMPP
  parseField _              = mzero

instance FromField Functionality where
  parseField "advertisement"     = return Advertisement
  parseField "audio-playback"    = return Audio
  parseField "audio"             = return Audio
  parseField "cloud-storage"     = return CloudStorage
  parseField "email"             = return Email
  parseField "file-download"     = return FileDownload
  parseField "file-sharing"      = return FileSharing
  parseField "file sharing"      = return FileSharing
  parseField "gaming"            = return Gaming
  parseField "IM and presence"   = return IMAndPrecense
  parseField "instant-messaging" = return InstantMessaging
  parseField "maps"              = return Maps
  parseField "media-playback"    = return Media
  parseField "media"             = return Media
  parseField "MMS"               = return MMS
  parseField "photo-sharing"     = return PhotoSharing
  parseField "social-networking" = return SocialNetworking
  parseField "social networking" = return SocialNetworking
  parseField "software-update"   = return SoftwareUpdate
  parseField "system"            = return System
  parseField "video-playback"    = return Video
  parseField "video"             = return Video
  parseField "weather"           = return Weather
  parseField "web-browsing"      = return WebBrowsing
  parseField "web browsing"      = return WebBrowsing
  parseField _                   = mzero

instance FromField Encapsulation where
  parseField = return . Encapsulation

instance FromField Encryption where
  parseField = return . Encryption

instance FromField ServiceProvider where
  parseField "Amazon"         = return Amazon
  parseField "Facebook"       = return Facebook
  parseField "Google"         = return Google
  parseField "Microsoft"      = return Microsoft
  parseField "Netflix"        = return Netflix
  parseField "P2P"            = return P2P
  parseField "Twitter"        = return Twitter
  parseField "Yahoo"          = return Yahoo
  parseField "YouTube"        = return YouTube
  parseField provider         = return $ Service provider

instance FromField ClientApp where
  parseField "Android Media Player" = return AndroidMediaPlayer
  parseField "Internet Explorer"    = return InternetExplorer
  parseField "Twitter"              = return TwitterApp
  parseField "Windows Media Player" = return WindowsMediaPlayer
  parseField "YouTube-player"       = return YouTubePlayer
  parseField app                    = return $ App app

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

instance NFData Transport where
instance NFData Application where
instance NFData Functionality where
instance NFData Encapsulation where
instance NFData Encryption where
instance NFData ServiceProvider where
instance NFData ClientApp where
instance NFData TerminalType where
instance NFData Vendor where
instance NFData OS where
instance NFData Direction where
instance NFData Object where

maybeRead :: Read a => String -> Maybe a
maybeRead xs =
  case reads xs of
    [(x, "")] -> Just x
    _         -> Nothing

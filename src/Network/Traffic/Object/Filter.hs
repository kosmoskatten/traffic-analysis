module Network.Traffic.Object.Filter
       ( FilterFunc (..)
       , filterObjects
       , filterFunc
       ) where

import qualified Data.Vector as V
import Network.Traffic.Object.Types ( EnumerationTarget (..)
                                    , Object (..)
                                    , ObjectVector
                                    , maybeRead )

newtype FilterFunc = FilterFunc (Object -> Bool)

instance Show FilterFunc where
  show _ = "(Object -> Bool)"

filterObjects :: Maybe FilterFunc -> ObjectVector -> ObjectVector
filterObjects Nothing objs               = objs
filterObjects (Just (FilterFunc f)) objs = V.filter f objs

filterFunc :: String -> String -> Either String FilterFunc
filterFunc target value =
  case maybeRead target of
    Just target' -> case target' >*< value of
                        Just func -> Right func
                        Nothing   -> Left "Cannot map value"
    Nothing      -> Left "Cannot map string to EnumerationTarget"

(>*<) :: EnumerationTarget -> String -> Maybe FilterFunc
(>*<) Transport       = mkFilterFunc transport
(>*<) Application     = mkFilterFunc application
(>*<) Functionality   = mkFilterFunc functionality
(>*<) ServiceProvider = mkFilterFunc serviceProvider
(>*<) ClientApp       = mkFilterFunc clientApp
(>*<) TerminalType    = mkFilterFunc terminalType

mkFilterFunc :: (Eq a, Read a)
                => (Object -> Maybe a) -> String -> Maybe FilterFunc
mkFilterFunc valueFrom value = do
  value' <- maybeRead value
  return $ FilterFunc (\obj -> maybe False (== value') (valueFrom obj))

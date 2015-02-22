module Network.Traffic.Object.Filter
       ( FilterFunc (..)
       , filterObjects
       , filterFunc
       ) where

import qualified Data.Vector as V
import Network.Traffic.Object.Types ( EnumerationTarget (..)
                                    , Object (..)
                                    , ObjectVector
                                    , Transport (..)
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
    Just target' -> case filterFunc' target' value of
                        Just func -> Right func
                        Nothing   -> Left "Cannot map value"
    Nothing      -> Left "Cannot map string to EnumerationTarget"

filterFunc' :: EnumerationTarget -> String -> Maybe FilterFunc
filterFunc' Transport value = do
  value' <- maybeRead value
  return $ FilterFunc (\obj -> maybe False (== value') (transport obj))

evalF :: String -> String -> Bool
evalF target value =
  case filterFunc target value of
    Right _ -> True
    Left _  -> False

module Main where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as LBS
import Network.Traffic.Object
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  file <- LBS.readFile =<< head `fmap` getArgs
  case decodeObjectsPar file of
    Right objects -> analyzeObjects objects                
    Left err      -> print err

analyzeObjects :: ObjectVector -> IO ()
analyzeObjects objects = do
  printQuantification "transport protocols" $
    quantifyEnumeration (enumerateByTransport objects)
  printQuantification "application protocols" $
    quantifyEnumeration (enumerateByApplication objects)

printQuantification :: String -> Quantification -> IO ()
printQuantification label (total, quantList) = do
  printf "Quantification of %s. Total # of items: %ld\n" label total
  forM_ quantList $ \(name, _, perc) ->
    printf " Item %s has %.2f percent.\n" (show name) perc

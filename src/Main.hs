module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forM_, forever)
import qualified Data.ByteString.Lazy as LBS
import Network.Traffic.Object
import System.Environment (getArgs)
import System.IO (hPutChar, hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  t    <- async ticker
  file <- LBS.readFile =<< head `fmap` getArgs
  case decodeObjectsPar file of
    Right objects -> do
      cancel t
      analyzeObjects objects                
    Left err      -> do
      cancel t
      print err

analyzeObjects :: ObjectVector -> IO ()
analyzeObjects objects = do
  printQuantification "transport protocols" $
    quantifyEnumeration (enumerateByTransport objects)
  printQuantification "application protocols" $
    quantifyEnumeration (enumerateByApplication objects)
  printQuantification "network functionality" $
    quantifyEnumeration (enumerateByFunctionality objects)
  printQuantification "service provider" $
    quantifyEnumeration (enumerateByServiceProvider objects)
  printQuantification "client app" $
    quantifyEnumeration (enumerateByClientApp objects)    

printQuantification :: String -> Quantification -> IO ()
printQuantification label (total, quantList) = do
  printf "\nQuantification of %s. Total # of items: %ld\n" label total
  forM_ quantList $ \(name, _, perc) ->
    printf " Item %s has %.2f percent.\n" (show name) perc

ticker :: IO ()
ticker =
  forever $ do
    threadDelay 1000000
    hPutChar stdout '.'
    hFlush stdout


module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Network.Traffic.Object
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- LBS.readFile =<< head `fmap` getArgs
  case decodeObjectsPar file of
    Right objects -> analyzeObjects objects                
    Left err      -> print err

analyzeObjects :: ObjectVector -> IO ()
analyzeObjects objects = do
  putStrLn $ "Length is: " ++ show (V.length objects)
  putStrLn $ "---------- enumerateByTransport"
  let trEnum = enumerateByTransport objects
  print (quantifyEnumeration trEnum)
  print trEnum
  
  putStrLn $ "---------- enumerateByApplication"
  let appEnum = enumerateByApplication objects
  print (quantifyEnumeration appEnum)
  print appEnum

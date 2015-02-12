module Main where

import Control.DeepSeq
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Network.Traffic.Object

main :: IO ()
main = do
  file <- LBS.readFile "hour.csv"
  file `deepseq` putStrLn $ "File read: " ++ show (LBS.length file)
  let result = decodeObjectsPar file
  case result of
    Right objects -> do
                putStrLn $ "Length is: " ++ show (V.length objects)
                print (enumerateByApplication objects)
    Left err      -> print err

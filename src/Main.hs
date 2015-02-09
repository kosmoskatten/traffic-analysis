module Main where

import Control.DeepSeq
import qualified Data.ByteString.Lazy as LBS
import Network.Traffic.Object

main :: IO ()
main = do
  file <- LBS.readFile "objects.csv"
  file `deepseq` putStrLn "File read"
  let result = decodeObjects file
  case result of
    Right objects -> print (enumerateByApplication objects)
    Left err      -> print err

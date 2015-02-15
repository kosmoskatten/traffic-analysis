module Main (main) where

import CommandLineParser (Command (..), parseCommandLine)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forM_, forever, void, when)
import qualified Data.ByteString.Lazy as LBS
import Network.Traffic.Object
import Repl (Repl, liftIO, runRepl)
import System.Console.Readline (readline, addHistory)
import System.Environment (getArgs)
import System.IO (hPutChar, hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = void $ runRepl repl Nothing
  {-t    <- async ticker
  file <- LBS.readFile =<< head `fmap` getArgs
  case decodeObjectsPar file of
    Right objects -> do
      cancel t
      analyzeObjects objects                
    Left err      -> do
      cancel t
      print err-}

repl :: Repl (Maybe ObjectVector) ()
repl = do
  line <- liftIO $ readline "> "
  case line of
    Nothing -> return ()
    Just line' -> do      
      continue <- handleCommandLine line'
      when continue repl

handleCommandLine :: String -> Repl (Maybe ObjectVector) Bool
handleCommandLine line =
  case parseCommandLine line of
    Left err      -> liftIO $ putStrLn (show err) >> return True
    Right command -> do
      liftIO $ addHistory line      
      handleCommand command

handleCommand :: Command -> Repl (Maybe ObjectVector) Bool
handleCommand (File filePath) = return True
handleCommand Help = return True
handleCommand Quit = return False

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

